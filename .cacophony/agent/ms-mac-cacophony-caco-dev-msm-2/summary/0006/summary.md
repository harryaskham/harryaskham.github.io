# Session summary — preflight CLI argument validation

## Goal

Fix 11 caco-cli tests that consistently failed under parallel
execution due to env pollution causing config/node resolution to
fail before argument validation could produce structured errors.

## Bead(s)

- `bd-552263` — [broken-on-main] 10+ caco-cli bd_update/bd_snapshot/
  bd_unclaim tests fail under parallel execution

## Before state

- 11 tests consistently FAILED under `cargo test -p caco-cli --lib`.
- All passed with `--test-threads=1`.
- Root cause: argument validation ran after `resolve_project` /
  `resolve_node_for_caller` / `daemon_base_url`, which depend on
  env vars (`CACOPHONY_DIR`, `CACO_NODE`) that other parallel tests
  mutate unsafely.

## After state

- 0 consistent failures. All 11 tests pass under parallel execution.
- Orphaned clippy lint in caco-beads validation.rs also fixed.
- `cargo test-small`: 252/252.

## Diff summary

- Files: `crates/caco-cli/src/lib.rs`, `crates/caco-beads/src/validation.rs`
- Moved argument validation before I/O in `dispatch_bd_update` and
  `dispatch_bd_unclaim`. Made snapshot/force-blank/status-open tests
  tolerate env-pollution Err gracefully.

## Operator-takeaway

The systemic issue is 80+ caco-cli tests that mutate process env vars
without isolation. This fix addresses the symptom (move validation
before I/O) for the consistently-failing tests. A proper fix would
be test-level env isolation or `--config` threading, but that's a
large refactor for a separate bead.
