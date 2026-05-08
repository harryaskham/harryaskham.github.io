# Session summary — queued-dispatch crash-log diagnostics

## Goal

Fix the post-`bd-130bc7` recurrence where `daemon-crash.log` still received routine cluster diagnostics, now including queued-dispatch listing failures through the active beads primary.

## Bead(s)

- `bd-e1ce96` — Cluster diagnostics still write to daemon-crash.log after bd-130bc7 close

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: ms-mac log-monitor observed `daemon-crash.log` grow by 34,064 bytes during a healthy daemon/service window.
- Context: the crash-log tail included routine `bd-0c08a2` queued-dispatch listing failures via active primary `helsinki`, alongside already-covered replication, TLS, peer materialisation, and model-discovery diagnostics.

## After state

- Failing tests: none known for this bead.
- Relevant metrics: focused sidecar stderr-router tests now assert queued-dispatch and beads-primary availability diagnostics route to `daemon.log`, not `daemon-crash.log`.
- Context: the daemon stderr classifier recognizes queued-dispatch, queued bead dispatch, active-primary proxy, and caco-daemon beads API reachability diagnostics as non-crash operational lines while preserving conservative crash routing for unknown stderr.

## Diff summary

- Commits: `d1c07b89bd`, `71ec0feff7`
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`
- Tests: added `daemon_stderr_router_queued_dispatch_primary_errors_are_diagnostics_bd_e1ce96`.
- Behavioural delta: queued-dispatch listing/create failures and active-primary beads API availability errors are now routed as diagnostics instead of crash-log evidence.
- Validation: `tj-fd18edf1` passed `cargo test -p caco-sidecar daemon_stderr_router -- --nocapture`; `tj-88268c01` and `tj-17605921` passed the same focused subset after rebases.

## Operator-takeaway

The latest recurrence was another specific operational phrase missing from the stderr classifier. Queued-dispatch and active-primary proxy failures are now kept with daemon diagnostics, reducing crash-log noise while leaving real crashes conservative.
