# Session summary — Post-b7775a cluster diagnostic crash-log routing

## Goal

Handle the log-monitor recurrence reporting routine replication and cluster diagnostics still appearing in `daemon-crash.log` after `bd-b7775a` closed. The session focused on expanding sidecar stderr routing coverage for the remaining pull/body-read diagnostics while preserving crash-log handling for real fatal evidence.

## Bead(s)

- `bd-f2c6b6` — Cluster diagnostics still write to daemon-crash.log after bd-b7775a close

## Before state

- The bead evidence showed `daemon-crash.log` growth after `bd-b7775a`, with non-crash diagnostics including full-state push timeouts, replication merge summaries, incremental pull state, peer direct-message materialization, profile discovery, TLS/cluster connection errors, and pull response/request body failures.
- Existing sidecar routing covered many of those classes, but pull response/body-read and pull request error shapes were not explicitly pinned.

## After state

- `crates/caco-sidecar/src/lifecycle.rs` now classifies full-state sync response body reads, pull response body reads, pull request errors, and generic response body reads as routine diagnostics.
- Added a post-`bd-b7775a` regression covering the full evidence cluster so it routes to daemon.log rather than `daemon-crash.log`.
- Focused validation passed: `tj-f91a6741` ran `cargo test -p caco-sidecar daemon_stderr_router_post_b7775a_cluster_diagnostics_bd_f2c6b6 -- --nocapture` successfully.

## Diff summary

- Commits: current branch commit for `bd-f2c6b6`; final landed squash SHA will be in the reintegration receipt.
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`
- Tests: +1 sidecar stderr-router regression for the post-`bd-b7775a` cluster diagnostics recurrence.
- Behavioural delta: routine pull/body-read and request diagnostics now join the already-covered replication/profile/peer-message diagnostics outside `daemon-crash.log`.

## Operator-takeaway

The remaining gap from the post-`bd-b7775a` crash-log sweep was pull/body-read style cluster diagnostics. Those are now explicitly classified as routine diagnostic output rather than crash evidence.
