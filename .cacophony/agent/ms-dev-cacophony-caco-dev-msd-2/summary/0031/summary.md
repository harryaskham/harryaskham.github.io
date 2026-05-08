# Session summary — AKS PID1 supervisor stale daemon pidfile self-heal

## Goal

Fix the AKS/container PID1 supervisor failure mode where `daemon.pid` can point at the parent `caco supervisor` process after the actual daemon child is gone. The goal was to keep fresh startup grace safe while making the stale parent-only pidfile restartable after grace expires.

## Bead(s)

- `bd-6ab18f` — Fix AKS PID1 supervisor stale daemon pidfile self-heal

## Before state

- Failing tests: none known for the focused new regression; production evidence showed `caco-daemon` stuck unhealthy with `owner_detail` pointing at `caco supervisor` and no daemon listener.
- Relevant metrics: `caco test run` job `tj-90e7129d` and retry `tj-7ec54d98` were used for the focused bd-6ab18f tests.
- Context: lifecycle warmup/backpressure logic treated any live daemon PID-file process as degraded/inspectable, so a stale parent supervisor PID could suppress daemon restart indefinitely.

## After state

- Failing tests: focused bd-6ab18f queued validation passed in `tj-7ec54d98`; broader `lifecycle::tests::` queued attempts `tj-a104adff` and `tj-0b491eee` hit retryable `daemon_restart_recovered` infrastructure outcomes, not test failures.
- Relevant metrics: 3 focused tests passed: command-line supervisor classification, status restartability after warmup, and convergence removal/restart of stale parent-supervisor pidfile.
- Context: stale `daemon.pid` entries pointing at `caco supervisor` remain warming during daemon grace, then status reports restartable `Stopped` and convergence removes the stale child pidfile before starting a new daemon.

## Diff summary

- Commits: `e3cf5de151`
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`, `SPEC.md`
- Tests: +3 focused lifecycle tests.
- Behavioural delta: `caco-daemon` pidfiles that point at the parent supervisor are now classified as `supervisor_pid_file`; after warmup they no longer trigger bounded-backpressure suppression and are cleaned up so lifecycle can restart the daemon.

## Operator-takeaway

AKS/PID1 supervisor deployments should no longer get stuck when the daemon child disappears but `daemon.pid` points at the supervisor parent. The node now waits during bounded startup grace, then self-heals through normal first-party lifecycle convergence.
