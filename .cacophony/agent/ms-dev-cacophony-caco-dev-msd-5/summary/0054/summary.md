# Session summary — Post-10a0e9 idle advisory regression guard

## Goal

Handle the latest log-monitor recurrence where persistent caco-aks idle advisory lines were still observed in `daemon-crash.log` after `bd-10a0e9` closed. The current code already routes timestamped nonfatal EventLogger lines as already-logged diagnostics, so this session pinned the exact post-close examples as a regression guard.

## Bead(s)

- `bd-1fb932` — caco-aks idle advisory still writes to daemon-crash.log after bd-10a0e9 close

## Before state

- Bead evidence showed timestamped warning lines such as `2026-05-08T19:38:48.475Z WARN [agent] persistent agent ms-mac-cacophony-caco-aks idle for <N>s (threshold 1800s) — advisory only; live runtime not auto-restarted without positive death evidence.` appearing in daemon-crash.log after `bd-10a0e9` closed.
- Main already included `bd-10a0e9`, which classifies `persistent_idle_advisory`, `advisory only; live runtime not auto-restarted`, and timestamped nonfatal EventLogger lines away from crash logging.

## After state

- Added an exact sidecar regression covering the post-`bd-10a0e9` timestamped idle-advisory shapes from the bead evidence.
- The regression asserts those lines route as `AlreadyLogged`, preserving daemon.log/feed diagnostics while keeping `daemon-crash.log` reserved for real crash evidence.
- Focused validation passed: `tj-27cd03d2` ran `cargo test -p caco-sidecar daemon_stderr_router_post_10a0e9_idle_advisory_shapes_bd_1fb932 -- --nocapture` successfully.

## Diff summary

- Commits: current branch commit for `bd-1fb932`; final landed squash SHA will be in the reintegration receipt.
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`
- Tests: +1 sidecar stderr-router regression for the post-`bd-10a0e9` persistent idle advisory recurrence.
- Behavioural delta: no new runtime branch was required beyond the already-landed classifier; the observed recurrence shape is now explicitly pinned.

## Operator-takeaway

The post-`bd-10a0e9` idle-advisory line is classified correctly by current code. If live logs still show it in `daemon-crash.log`, the likely remaining issue is deployment/runtime convergence rather than another classifier branch.
