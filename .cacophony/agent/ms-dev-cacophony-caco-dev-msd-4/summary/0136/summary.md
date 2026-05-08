# Session summary — protect daemon during child-service port cleanup

## Goal

Fix the lifecycle stale-port cleanup path so service-scoped restarts of child/pid-only services cannot kill the active or restarting `caco-daemon` when those child services share the daemon listener or when the daemon is intentionally serving the caco-web dashboard port.

## Bead(s)

- `bd-d9ecec` — caco up kills daemon when daemon PID holds caco-web port 11180

## Before state

- Failing tests: no local regression covered the 2026-05-06 recurrence where `caco restart --service caco-tts-daemon` treated the daemon listener port as a stale child-service port.
- Relevant context: caco-web external-port cleanup already had daemon PID protections, but shutdown cleanup still ran generic `ensure_port_free` for service ports. PID-only services such as TTS/STT use the parent daemon address as `service_addr`, so a scoped child restart could attempt to free the daemon control port.
- Evidence from the bead: prior recovery output showed stale-port cleanup killing daemon PIDs on ports 11180 and 11100.

## After state

- Failing tests: none from focused validation.
- Relevant metrics: final post-rebase queued validation `tj-81d792af` passed `cargo test -p caco-sidecar bd_d9ecec -- --nocapture` with 2 tests passing.
- Context: shutdown service-port cleanup now skips the generic service-port sweep for PID-only children, while non-PID services still use the protected cleanup helper. The caco-web external-port protection path remains covered.

## Diff summary

- Commits: `63717bc6e6`
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`
- Tests: +1 focused regression for scoped TTS child shutdown preserving the shared daemon listener; existing caco-web daemon-owned port regression still passes under the same bead filter.
- Behavioural delta: service-scoped child cleanup no longer converts a degraded pid-only service restart into a daemon outage by killing the parent daemon listener owner.

## Operator-takeaway

The recurrence broadened the original caco-web safety invariant to all pid-only child services. The fix keeps daemon-owned ports protected in child-service lifecycle paths while preserving stale-daemon cleanup for explicit daemon shutdown/replacement.
