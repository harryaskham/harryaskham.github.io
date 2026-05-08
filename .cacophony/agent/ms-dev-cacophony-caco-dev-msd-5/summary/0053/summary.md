# Session summary — Cluster diagnostic crash-log routing

## Goal

Handle the post-`bd-1a8412` recurrence where daemon-crash.log still received routine cluster/replication diagnostics. This session focused on the replication merge-summary shape that was not covered by the earlier `push merge from` / `pull merge from` classifier phrases.

## Bead(s)

- `bd-7c8172` — Cluster diagnostics still write to daemon-crash.log after bd-1a8412 close

## Before state

- Log-monitor reported daemon-crash.log growth after `bd-1a8412` closed, with routine diagnostics including replication/push merge summaries, incremental pulls, full-state push timeouts, queued-dispatch listing failures, TLS/cluster connection errors, peer message materialization, and model discovery 404s.
- Existing sidecar routing already covered many of those categories, plus queued-dispatch/beads-primary errors from `bd-e1ce96`, but slash-style replication summaries like `ms-dev/sgu24 push merge: ...` and `helsinki/ms-dev pull merge: ...` were not matched by the `push merge from` / `pull merge from` patterns.

## After state

- The sidecar daemon-stderr classifier now treats `push merge:` and `pull merge:` replication summary lines as routine daemon diagnostics.
- Added a focused regression for slash-style push and pull merge summaries to ensure they route to daemon.log rather than daemon-crash.log.
- Updated SPEC/README/AGENTS guidance to mention routine cluster diagnostics and replication merge summaries as non-crash diagnostics.
- Focused validation passed: `tj-4af82a3e` ran `cargo test -p caco-sidecar daemon_stderr_router_replication_summaries_are_diagnostics_bd_7c8172 -- --nocapture` successfully.

## Diff summary

- Commits: current branch commit for `bd-7c8172`; final landed squash SHA will be in the reintegration receipt.
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`, `SPEC.md`, `README.md`, `AGENTS.md`
- Tests: +1 sidecar stderr-router regression for slash-style replication merge summaries.
- Behavioural delta: replication merge summary stderr lines now go to daemon.log diagnostic routing instead of daemon-crash.log.

## Operator-takeaway

This narrows the latest crash-log recurrence to another concrete routine-diagnostic phrase. The crash log should no longer collect slash-style replication merge summaries while still preserving unknown or genuinely fatal stderr as crash evidence.
