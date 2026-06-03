# Session summary — scope per-node project-checkout materialization (bd-eca3f8)

## Goal

Stop the daemon from cloning every configured project on every node. A node
that only runs cacophony + picasso agents was still materializing unrelated
project checkouts (ws-health-scratch, mono, collective, tendril, etc.), wasting
~3GB and clone time per node. Scope materialization to the projects that
actually have agent activity on the local node, while never withholding a
checkout a node legitimately needs.

## Bead(s)

- `bd-eca3f8` — daemon: scope per-node project-checkout materialization to declared projects only (efficiency) (claimed, fixed, this session)

## Before state

- Failing tests: none (efficiency gap, not a test failure).
- `CheckoutManager::new_with_config` built checkout state for ALL
  `config.projects` and `init_all()` cloned them on every node regardless of
  per-node agent declarations.
- Context: downgraded from outage-critical to efficiency now that state disks
  are 200GB (was the disk-full root cause at 8GB).

## After state

- Passing: `cargo check -p caco-config -p caco-daemon --tests`,
  `cargo clippy -p caco-config -p caco-daemon --lib -- -D warnings`, and the new
  `persistent::tests::projects_for_node_materialization` unit test (all via queued caco test run).
- New `Config::projects_for_node_materialization(node)` returns the set of
  projects to materialize: every project bound to a resolved persistent agent on
  the node (node-scoped explicit bindings + all applicable project-scoped
  declarations) plus `default_project`. Returns `None` (materialize-all
  fallback) when no projects are configured or no positive scoping signal
  exists, so the optimization never strands a needed checkout.
- Daemon startup now calls `CheckoutManager::new_with_config_for_node(.., node_name)`;
  the original `new_with_config` (materialize-all) is retained unchanged for CLI
  inspection and existing tests.

## Diff summary

- Code/content commit: `26b3152be7` (final landed squash SHA from the reintegration receipt).
- Summary artefact commit: intentionally omitted (must not self-reference its own mutable SHA).
- Files touched:
  - `crates/caco-config/src/model.rs` (+ projects_for_node_materialization helper)
  - `crates/caco-daemon/src/checkout.rs` (+ new_with_config_for_node, doc on new_with_config)
  - `crates/caco-daemon/src/lib.rs` (daemon startup uses node-scoped constructor)
  - `crates/caco-daemon/src/persistent.rs` (+ unit test)
- Tests: +1 (projects_for_node_materialization).
- Behavioural delta: per-node checkout materialization is scoped to declared
  projects + default_project; falls back to materialize-all when scoping is
  indeterminate. No change for single-project or default_project-covered nodes.

## Operator-takeaway

Nodes now clone only the project checkouts they actually use (declared agent
projects + default_project), not the entire project list. The fallback is
deliberately conservative: any ambiguity (no projects configured, or no
declarations and no default_project) reverts to the old materialize-all
behavior, so this can never withhold a checkout a node needs. Lazy
agent-spawn checkout paths remain unaffected.
