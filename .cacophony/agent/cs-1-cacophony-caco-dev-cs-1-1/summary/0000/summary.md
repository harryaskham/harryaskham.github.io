# Session summary — Propagate partial-clone config into agent checkouts (bd-0b0d80)

## Goal

Fix the cs-1 infra bug where agent checkouts on a constrained microvm node
intermittently cannot commit. The daemon canonical checkout is a blobless
partial clone, agent checkouts borrow its object store via alternates but have
no partial-clone awareness, so a full-tree `git commit` that references an
un-materialized promisor blob aborts with a bare "invalid object … Error
building trees" and makes no attempt to fetch the missing object. This blocked
every commit/reintegration in the child until the canonical re-hydrated.

## Bead(s)

- `bd-0b0d80` — Blobless partial-clone canonical checkout intermittently blocks commits on constrained microvm nodes (cs-1)

## Before state

- Failing tests: none (pre-existing). bd-0b0d80 open/unassigned, filed as design/forensic.
- Mechanism confirmed live on cs-1: canonical has `remote.origin.promisor=true`,
  origin url `ssh://git@github.com/harryaskham/cacophony.git`, format v1; the
  `git clone --shared` child is format v0 with NO promisor config, origin = local
  canonical path. A missing borrowed blob → hard "invalid object", no fetch attempt.
- `create_shared_clone` (crates/caco-daemon/src/agent/spawn.rs) did not propagate
  any partial-clone config to the child.

## After state

- Failing tests: none. 5 create_shared_clone tests pass (2 new); clippy clean for caco-daemon (--lib, 0/0).
- `create_shared_clone[_with_default_branch]` now calls `propagate_partial_clone_config`,
  which — only when the canonical is detected as a promisor/partial clone — wires a
  dedicated `promisor-upstream` remote pointing at the canonical's TRUE upstream URL,
  marks it promisor + carries the clone filter, then flips the child to format v1 +
  `extensions.partialClone` LAST. Best-effort: full-clone canonicals untouched; any
  failure leaves a plain shared clone and never aborts checkout creation.
- Result: a missing borrowed blob now lazy-fetches from the real upstream (e.g. GitHub)
  instead of failing the commit.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched:
  - `crates/caco-daemon/src/agent/spawn.rs` — new `git_config_value` +
    `propagate_partial_clone_config` helpers, call site in
    `create_shared_clone_with_default_branch`, 2 new unit tests + a test helper.
  - `SPEC.md` §16.2 — new checkout-strategy rule documenting the propagation contract.
- Tests: +2 (`create_shared_clone_propagates_partial_clone_config_from_promisor_canonical`,
  `create_shared_clone_leaves_full_clone_canonical_untouched`).
- Behavioural delta: agent checkouts derived from a blobless/partial canonical can now
  lazy-fetch un-materialized borrowed blobs from upstream; full-clone canonicals and all
  other projects/nodes are unaffected.

## Operator-takeaway

The hard "invalid object / Error building trees" commit failures on constrained
microvm nodes (cs-1) were caused by `--shared` agent clones inheriting the canonical's
borrowed objects but not its promisor remote, so they could never fetch a blob the
canonical hadn't materialized. Agent checkouts now carry a `promisor-upstream` remote
to the true upstream and will lazy-fetch instead of failing — when the network is
reachable this eliminates the failure entirely; when it isn't, it degrades to a fetch
attempt rather than an immediate hard error. Note this only changes NEW agent checkouts
on partial-clone-canonical nodes; the bigger open question (whether constrained nodes
should use full clones for the canonical itself) is intentionally left for a follow-up.
