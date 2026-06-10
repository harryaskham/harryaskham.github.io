# bd-3dce60 — Silence pre-existing clippy::too_many_arguments on release_failed_agent_bead_blocking

## Bead
bd-3dce60 (clippy/daemon/hygiene, P4; bd-b03101 follow-up). po4-1 flagged this pre-existing warning during bd-0c755b: `async fn release_failed_agent_bead_blocking` (crates/caco-daemon/src/beads.rs, from bd-b03101's reconcile-path spawn_blocking offload) has 8 args (one over clippy's default-7 threshold) → `clippy::too_many_arguments`. Not gate-blocking (the merge gate runs clippy --workspace without -D warnings), but it kept clippy --workspace from being fully clean.

## Change
Added `#[allow(clippy::too_many_arguments)]` to the function (with a one-line rationale comment). The 8 args are all coherent reconcile-path inputs (store/state/bead identity + the three landed-state flags `agent_completed`/`completed_landed`/`is_permanent`); grouping them into a struct would churn the call site for no benefit, so the targeted allow is the minimal idiomatic fix.

## Validation
- `cargo clippy -p caco-daemon --lib` (tj-df943b69): PASSED, 0 warnings — the `too_many_arguments` warning on `release_failed_agent_bead_blocking` is silenced.
- `git diff --check` clean (4-line change).

## Diff
See the reintegration receipt for the landed squash SHA.
