# Session summary — Shorten crash-log diagnostic decay

## Goal

Finish `bd-41e1a5` by addressing the post-`bd-c381f2` recurrence where known TTS/STT watchdog diagnostics still appeared in `daemon-crash.log` during the next log-monitor sweep. The intent was to make already-classified non-crash noise disappear quickly through normal daemon rotation rather than waiting for the previous ten-minute rotation interval.

## Bead(s)

- `bd-41e1a5` — TTS/STT watchdog respawns recur in daemon-crash.log after bd-c381f2 close

## Before state

- Failing tests: none known before this slice.
- Relevant metrics: log-monitor counted `watchdog_stale=20` in `daemon-crash.log` for the 2026-05-09T03:21:12Z to 2026-05-09T03:36:12Z window, with examples still citing `bd-5f8223` after `bd-c381f2` closed.
- Context: `bd-c381f2` taught crash-log rotation to prune known non-crash diagnostics, but the periodic rotation loop only ran every ten minutes, leaving stale watchdog lines visible across the next monitor sweep.

## After state

- Failing tests: none from focused validation.
- Relevant metrics: queued validation passed after rebase: `tj-4dc411a5` for `rotate_crash_log_removes_post_c381f2_watchdog_noise_bd_41e1a5` and `bj-ecff8f23` for `cargo check -p caco-daemon --lib`.
- Context: the crash-log rotation loop now runs every 60 seconds, so known non-crash lines pruned by `crash_log::rotate_crash_log` decay between log-monitor sweeps. A regression test covers the exact post-`bd-c381f2` watchdog shapes from the bead description.

## Diff summary

- Commits: `bd97d706db`.
- Files touched: `crates/caco-daemon/src/lib.rs`, `crates/caco-daemon/src/crash_log.rs`.
- Tests: +1 daemon unit test / -0 / flipped 0.
- Behavioural delta: stale watchdog respawn diagnostics already present in `daemon-crash.log` should now be pruned within roughly a minute of daemon uptime instead of persisting for another ten-minute monitoring window.

## Operator-takeaway

The recurrence was caused by cleanup cadence, not a new unclassified watchdog text shape: the pruning logic existed, but rotation was too slow for the monitor’s burn-down sweep cadence. This slice tightens that cadence and locks the observed watchdog lines into regression coverage.
