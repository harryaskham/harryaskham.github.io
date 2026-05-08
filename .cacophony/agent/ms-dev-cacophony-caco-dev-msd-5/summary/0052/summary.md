# Session summary — Legacy persistent idle advisory stderr routing

## Goal

Address the post-`bd-6dacb0` recurrence where persistent caco-aks idle advisory lines still appeared in `daemon-crash.log`. The intent was to cover both the timestamped EventLogger line and the older direct-stderr advisory shape so routine idle diagnostics cannot masquerade as crash evidence.

## Bead(s)

- `bd-faa411` — caco-aks idle advisory still writes to daemon-crash.log after bd-6dacb0 close

## Before state

- Log-monitor observed 52 timestamped `persistent_idle_advisory` WARN lines in both `daemon.log` and `daemon-crash.log` during a bounded 15-minute sweep after `bd-6dacb0` closed.
- The daemon handler already emitted current advisories via `log_without_stderr_mirror` and rate-limited them, but the sidecar classifier did not explicitly treat legacy/non-timestamped persistent idle advisory stderr as routine diagnostics.
- That left a recurrence gap for older direct-stderr bypass lines such as the previous `bd-db327a: persistent idle advisory ... no auto-restart...` shape.

## After state

- The sidecar daemon-stderr router now treats lines containing `persistent idle advisory` or `live runtime not auto-restarted without positive death evidence` as routine daemon diagnostics, not crash evidence.
- Added a focused regression covering both the observed timestamped post-close advisory line (`AlreadyLogged`) and the legacy direct-stderr bypass line (`DaemonLog`).
- Documentation/spec guidance now states that persistent idle advisories, including legacy/non-timestamped bypass lines, belong in daemon.log/feed diagnostics rather than `daemon-crash.log` unless positive death evidence exists.
- Focused validation passed: `tj-0b1bdb95` ran `cargo test -p caco-sidecar daemon_stderr_router_keeps_persistent_idle_advisory_out_of_crash_log_bd_faa411 -- --nocapture` successfully.

## Diff summary

- Commits: current branch commit for `bd-faa411`; final landed squash SHA will be in the reintegration receipt.
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`, `SPEC.md`, `README.md`, `AGENTS.md`
- Tests: +1 sidecar stderr-router regression covering persistent idle advisory crash-log recurrence.
- Behavioural delta: if any persistent idle advisory still reaches daemon stderr, the sidecar routes it as non-crash diagnostic context rather than appending it to `daemon-crash.log`.

## Operator-takeaway

This closes the remaining crash-log path around persistent idle advisories: current daemon code avoids stderr mirroring, and sidecar routing now catches legacy/advisory stderr shapes if they appear during rollout or from older processes.
