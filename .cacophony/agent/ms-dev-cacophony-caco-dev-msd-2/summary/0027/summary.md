# Session summary — deleted dependency unblock

## Goal

Fix the bead dependency resolver so beads are not permanently blocked when a dependency bead has been deleted or removed from the board index. The intended operator outcome is that normal ready-bead burn-down can continue even after dependency cleanup or destructive reconciliation removes a blocker row.

## Bead(s)

- `bd-5682d8` — Fix beads blocked by deleted dependencies

## Before state

- Failing tests: none known for this bead before implementation.
- Relevant metrics: dependency resolution treated a missing dependency row as blocking, so a downstream bead could stay computed-blocked forever if its blocker was removed instead of closed.
- Context: the store already treated explicit `closed` and `deleted` dependency statuses as resolved, and `find_newly_unblocked_after_delete` covered tombstoned deleted dependencies, but not missing rows.

## After state

- Failing tests: none in the focused validation run. One queued crate-suite attempt (`tj-42bb4af0`) was an infrastructure-only daemon-restart recovery and passed on retry.
- Relevant metrics: `cargo test -p caco-beads -- --test-threads=2` passed as `tj-23846534`; focused tests passed as `tj-227bfb`, `tj-5339f19d`, and `tj-3c74f7b8`.
- Context: missing dependency rows now resolve the same way as `deleted` dependencies, so downstream beads expose empty `open_dependency_ids`, appear in `list_ready`, and can be claimed.

## Diff summary

- Commits: `27dbe58de2` (`bd-5682d8: unblock beads with deleted dependencies`)
- Files touched: `crates/caco-beads/src/store.rs`, `crates/caco-beads/src/model.rs`, `SPEC.md`
- Tests: +1 store regression (`missing_deleted_dependency_unblocks_child`) plus updated resolver expectation; no tests removed.
- Behavioural delta: `BeadsStore::is_blocking_status(None)` now treats missing dependency rows as resolved/deleted. The live `is_blocked` / `open_dependency_ids`, ready-list, and claim paths all inherit that single predicate.

## Operator-takeaway

Deleted or missing dependency beads no longer strand downstream work in a permanently blocked state; the board can recover naturally after cleanup/reconcile removes a blocker row, while still treating unknown status strings as conservative blockers.
