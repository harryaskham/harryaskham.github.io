# Session summary — Suppress already-covered log-monitor recurrences

## Goal

Stop the caco-aks idle-advisory crash-log recurrence loop from generating a fresh implementation bead every sweep when the current mainline classifier already covers the exact observed warning shape. The aim was to move this from repeated code-gap filings to log-monitor aggregation/rollout-convergence tracking.

## Bead(s)

- `bd-b8d4a7` — caco-aks idle advisory still writes to daemon-crash.log after bd-890da1 close

## Before state

- The log monitor filed another caco-aks idle-advisory recurrence immediately after `bd-890da1` closed, with the same timestamped warning shape already routed by current sidecar logic as non-crash diagnostics.
- The repeated recurrence beads were causing implementation churn even though the live evidence pointed at rollout/runtime convergence or duplicate live stderr routing.

## After state

- `.cacophony/profiles/log-monitor.md` now explicitly instructs log monitors to check whether latest landed code/profile already covers an observed post-close recurrence before filing a fresh implementation bead.
- Already-covered live-log recurrences should be aggregated under `recurring_signatures`, reference the root-cause bead, and file at most one follow-up draft/blocked tracker for suppression or deployment convergence.
- `crates/caco-profile/src/lib.rs` pins this guidance in the existing log-monitor profile regression test.

## Diff summary

- Commits: current branch commit for `bd-b8d4a7`; final landed squash SHA will be in the reintegration receipt.
- Files touched: `.cacophony/profiles/log-monitor.md`, `crates/caco-profile/src/lib.rs`
- Tests: strengthened the log-monitor profile test to require the new already-covered recurrence guidance.
- Validation: `tj-0c581212` ran `cargo test -p caco-profile log_monitor_persistent_stack_excludes_worker_lifecycle_mixins -- --nocapture` successfully. An earlier broad filter `tj-82d73066` matched zero tests, so it was not used as the evidence.

## Operator-takeaway

This fixes the workflow loop rather than adding yet another identical classifier branch: log-monitor should stop filing new implementation beads for already-covered live log shapes and instead treat them as convergence/suppression evidence.
