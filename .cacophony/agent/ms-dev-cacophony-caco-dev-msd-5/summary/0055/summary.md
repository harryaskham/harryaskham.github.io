# Session summary — Post-1fb932 idle advisory regression guard

## Goal

Handle the newest log-monitor recurrence reporting persistent caco-aks idle advisory warnings in `daemon-crash.log` after `bd-1fb932` closed. The current sidecar routing already treats timestamped nonfatal EventLogger lines as already-logged diagnostics, so this session pinned the new observed timestamps as explicit regression coverage and preserved the live-deployment-convergence diagnosis.

## Bead(s)

- `bd-890da1` — caco-aks idle advisory still writes to daemon-crash.log after bd-1fb932 close

## Before state

- Bead evidence showed timestamped warning lines at `2026-05-08T19:56:17.621Z` and `2026-05-08T19:56:31.387Z` recurring in `daemon-crash.log` after `bd-1fb932` closed.
- Main already included routing for timestamped nonfatal daemon log lines plus the persistent idle advisory phrases, so the observed shape should classify as `AlreadyLogged` in current code.

## After state

- Added a regression test with the exact post-`bd-1fb932` idle-advisory timestamps from the bead evidence.
- The test asserts those lines route as `DaemonStderrRoute::AlreadyLogged`, keeping daemon.log/feed visibility without appending to `daemon-crash.log`.
- Focused validation passed: `tj-5f037edd` ran `cargo test -p caco-sidecar daemon_stderr_router_post_1fb932_idle_advisory_shapes_bd_890da1 -- --nocapture` successfully.

## Diff summary

- Commits: current branch commit for `bd-890da1`; final landed squash SHA will be in the reintegration receipt.
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`
- Tests: +1 sidecar stderr-router regression for post-`bd-1fb932` persistent idle advisory recurrence.
- Behavioural delta: no additional routing branch was necessary because current code already classifies the observed warning shape away from crash logging.

## Operator-takeaway

The newest observed idle-advisory line is covered by current classifier logic. Continued live `daemon-crash.log` growth for this exact shape points to rollout/runtime convergence or duplicate live stderr routing, not an uncovered message pattern.
