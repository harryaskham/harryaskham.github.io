# Session summary — bd-4ff341 partial-clone-safe agent rebase

## Goal

Fix first-party `caco agent rebase` on blobless / `blob:none` partial-clone checkouts where the agent checkout's `origin` points at a local canonical mirror, but the worker itself may be missing propagated partial-clone config. In that cs-2 shape, direct GitHub fetches can fail with promisor blob-by-SHA errors even though the local canonical has or can hydrate the needed objects.

## Bead(s)

- `bd-4ff341` — First-party rebase fails fetching blob from GitHub on blob-none partial-clone checkouts (cs-2)

## Before state

- Existing `resolve_agent_rebase_target` detected partial clones only from the agent checkout itself.
- If the worker checkout lacked `extensions.partialClone` / `remote.origin.promisor` / `remote.origin.partialclonefilter`, it fetched the rebase target directly from the canonical checkout's true upstream URL (GitHub).
- On cs-2 blobless local canonical checkouts, GitHub can refuse arbitrary promisor blob fetches by SHA (`not our ref ...`, `could not fetch ... from promisor remote`), blocking first-party rebase even though the local canonical checkout is the safer source.

## After state

- `resolve_agent_rebase_target` now treats the checkout as partial-clone-sensitive when either:
  - the agent checkout itself has partial/promisor signals, or
  - its local canonical `origin` is a non-bare checkout with partial/promisor signals.
- In either case, first-party rebase fetches through the local canonical path and updates the usual rebase/tracking refs from that local source.
- Full-clone workers backed by full local canonicals preserve the previous bd-574564 behaviour of fetching the true upstream URL, so they do not rebase onto a stale local canonical by default.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-cli/src/lib.rs`
- Behavioural delta:
  - broadened partial-clone-safe rebase target selection to include a partial/promisor local canonical mirror even when older workers lack propagated child config.
  - extended the existing bd-590f73 regression to cover the cs-2 / bd-4ff341 shape.

## Validation

- `cargo test -p caco-cli agent_rebase_partial_clone_worker_fetches_through_local_canonical_bd_590f73 --lib` passed.
- `cargo check -p caco-cli --lib` passed.
- `cargo clippy -p caco-cli --lib -- -D warnings` passed.
- `./scripts/rustfmt-changed.sh crates/caco-cli/src/lib.rs` intentionally skipped the large crate root because its HEAD version is not rustfmt-clean; no unrelated formatting churn was introduced.

## Operator-takeaway

First-party rebase should no longer route blobless local-canonical-backed workers through GitHub just because the child checkout lacks propagated partial-clone config. It now recognizes the partial canonical mirror itself and uses that local path for a partial-clone-safe fetch/rebase target.
