# Session summary — Watchdog diagnostic stderr replay guard

## Goal

Stop TTS/STT pid-only watchdog stale-process diagnostics from continuing to populate `daemon-crash.log` after `bd-1826db`, including the replay path where old watchdog stderr tails with stale bead references are wrapped in a startup previous-stderr banner.

## Bead(s)

- `bd-d1cf99` — TTS/STT watchdog respawns recur in daemon-crash.log after bd-1826db close

## Before state

- Failing tests: none known before this slice.
- Relevant metrics: log-monitor reported `daemon-crash.log` at 1,152,074 bytes with 44 timestamped lines in the 2026-05-09T02:21:29Z to 2026-05-09T02:36:29Z sweep; 24 were watchdog stale-process diagnostics.
- Context: `bd-1826db` routed direct sidecar watchdog diagnostics to service logs, but deployed hosts still had historical or replayed `[tts-watchdog] ... (bd-5f8223)` lines being mirrored as daemon stderr/crash evidence.

## After state

- Failing tests: none from focused validation.
- Relevant metrics: queued validation passed after rebase: `tj-0c505981` for daemon stderr-mirror tests and `bj-d9882933` for `cargo check -p caco-daemon --lib`.
- Context: the daemon stderr mirror guard now recognizes pid-only watchdog diagnostics by stable diagnostic marker and known watchdog text shape, including stale previous-stderr tails with old `bd-5f8223` text, while preserving panic-shaped service errors.

## Diff summary

- Commits: `632e79bdd1`.
- Files touched: `crates/caco-daemon/src/logging.rs`.
- Tests: +1 daemon unit test / -0 / flipped 0.
- Behavioural delta: warning-only TTS/STT watchdog stale-process diagnostics no longer re-enter stderr/crash-log through daemon logging or startup previous-stderr replay, but actual panic-shaped watchdog/service failures still mirror.

## Operator-takeaway

This recurrence was another replay/source-side guard gap rather than a new fatal service crash. The fix makes watchdog diagnostic suppression content-based at the daemon stderr mirror boundary so stale deployed crash-log tails decay instead of being re-emitted.
