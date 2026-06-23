# Session summary — bd-88cb88 (testable PR-backend reintegration dispatch)

## Goal

Guard the bd-571549 fix (pr_auto_merge must actually MERGE, not just open the PR)
with a unit test. The per-ProjectPrBackendMode dispatch was duplicated inline in
two large CLI functions and was not unit-testable, so bd-571549 relied only on
daemon squash tests + the 'life' pilot e2e. Extract the routing into a pure,
tested helper that both dispatch blocks use.

## Bead(s)

- `bd-88cb88` — [test-hygiene] Add unit test for reintegration backend-mode
  dispatch (AutoMerge→merge+verify) — bd-571549 follow-up.

## Before state

- Failing tests: none from this scope. (Note: an UNRELATED broken-on-main —
  caco-tui speech_popup, bd-6ab0e3 — was blocking the cacophony-fast-tests gate
  fleet-wide while this landed; tui-md2-0 owned that fix.)
- `caco agent complete` (lib.rs) + `caco agent reintegrate` (lib.rs) each had a
  byte-identical inline `match project_pr_backend_mode { DirectMerge | AutoMerge
  | Review | None }` dispatch; the AutoMerge→merge+verify routing (bd-571549) had
  no unit-test guard.

## After state

- Failing tests: none. `cargo check --workspace --tests` green (tj-fb55030e);
  caco-cli routing tests 11/11 incl the new test + the existing
  `project_pull_request_backend_routes_direct_and_pr_modes` (tj-c139a2a0).
- New `ReintegrationBackendPath` enum (OpenMergeVerify / OpenOnly /
  LocalReintegrate) + pure `reintegration_path_for_backend_mode(backend)`. Both
  dispatch blocks route through it, behavior-identical (DirectMerge + AutoMerge →
  OpenMergeVerify, with AutoMerge still conditionally setting `outcome.mode =
  mode`; Review → OpenOnly; None → LocalReintegrate). The routing decision is now
  deduped + unit-tested.

## Diff summary

- Code/content commit: 2cac0c13a (final landed squash SHA from the reintegration
  receipt).
- Summary artefact commit: intentionally omitted (no self-reference).
- Files touched: crates/caco-cli/src/lib.rs (+123/-68 — the helper + both
  rewires net-reduce the duplicated dispatch).
- Tests: +1 (`reintegration_path_for_backend_mode_routes_automerge_to_merge_verify_bd_88cb88`).
- Behavioural delta: none — the rewire is exactly equivalent to the two prior
  inline matches; only the dispatch is now factored + guarded.

## Operator-takeaway

The bd-571549 fix (AutoMerge merges, not open-only) is now guarded by a fast unit
test asserting AutoMerge → OpenMergeVerify, so a future edit to the reintegration
dispatch can't silently regress pr_auto_merge back to reporting success the
instant the PR opens. The routing is a pure function both `agent complete` and
`agent reintegrate` share, so the two paths can no longer drift apart.
