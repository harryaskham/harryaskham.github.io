# Session summary — idle-advisory action stderr recurrence

## Goal

Fix the post-`bd-faa411` recurrence where persistent `caco-aks` idle advisory lines still appeared in `daemon-crash.log` despite being warning-only operational state for a live persistent agent.

## Bead(s)

- `bd-10a0e9` — caco-aks idle advisory still writes to daemon-crash.log after bd-faa411 close

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: ms-mac log-monitor observed `daemon-crash.log` grow by 57,091 bytes with 47 timestamped `persistent_idle_advisory` WARN lines during a healthy daemon/service window.
- Context: source-side idle advisory logging is already rate-limited and uses non-stderr-mirrored logging, but the sidecar stderr classifier still needed coverage for the post-`bd-faa411` observed line shape and action-only `persistent_idle_advisory` metadata in legacy/bypass stderr.

## After state

- Failing tests: none known for this bead.
- Relevant metrics: focused sidecar stderr-router tests assert post-`bd-faa411` timestamped idle advisories are treated as already logged, and action-only `persistent_idle_advisory` diagnostics route to `daemon.log` rather than `daemon-crash.log`.
- Context: the classifier now treats `persistent_idle_advisory` action metadata and the exact warning-only live-runtime advisory phrase as operational diagnostics, while keeping unknown stderr conservative.

## Diff summary

- Commits: `87ae6e965d`
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`
- Tests: added `daemon_stderr_router_post_faa411_idle_advisory_shapes_bd_10a0e9`.
- Behavioural delta: post-`bd-faa411` persistent idle advisory recurrence shapes stay out of `daemon-crash.log`.
- Validation: `tj-fb096388` passed `cargo test -p caco-sidecar bd_10a0e9 -- --nocapture`; `tj-a93c47cf` passed `cargo test -p caco-sidecar daemon_stderr_router -- --nocapture`; `tj-7c968f82` passed the same focused subset after rebase.

## Operator-takeaway

The latest idle-advisory recurrence is covered at the sidecar routing layer: timestamped advisory mirrors are dropped as already logged, and explicit `persistent_idle_advisory` action diagnostics are routed as daemon diagnostics rather than crash evidence.
