# Session summary — Component-only watchdog respawn guard

## Goal

Stop TTS/STT watchdog respawn diagnostics from continuing to populate `daemon-crash.log` after `bd-d1cf99`, including the source-side path where the daemon logs `component = tts-watchdog` with a bare message such as `respawned caco-stt-daemon.ms-mac` that does not include the bracketed `[tts-watchdog]` prefix or stable diagnostic marker.

## Bead(s)

- `bd-adb882` — TTS/STT watchdog respawns recur in daemon-crash.log after bd-d1cf99 close

## Before state

- Failing tests: none known before this slice.
- Relevant metrics: log-monitor reported `daemon-crash.log` at 1,150,496 bytes with 46 timestamped lines in the 2026-05-09T02:36:35Z to 2026-05-09T02:51:35Z sweep; 31 were watchdog stale-process diagnostics.
- Context: prior guards recognized bracketed `[tts-watchdog]` lines and `diagnostic=pid-only-watchdog`, but the daemon source-side respawn notice passed `tts-watchdog` as the component and `respawned ...` as plain message text.

## After state

- Failing tests: none from focused validation.
- Relevant metrics: queued validation passed after rebase: `tj-747f50d8` for the focused daemon watchdog stderr-mirror test and `bj-e0ac06d5` for `cargo check -p caco-daemon --lib`.
- Context: the stderr mirror guard now includes the component name when classifying watchdog diagnostics, so component-only respawn messages are suppressed alongside bracketed/replayed watchdog tails while panic-shaped service errors still mirror.

## Diff summary

- Commits: `71b20a96e8`.
- Files touched: `crates/caco-daemon/src/logging.rs`.
- Tests: extended 1 daemon unit test / -0 / flipped 0.
- Behavioural delta: warning-only TTS/STT watchdog respawn diagnostics in component/message form stay out of daemon stderr/crash-log; real panic-shaped watchdog/service output remains crash-visible.

## Operator-takeaway

The post-`bd-d1cf99` recurrence was the component-only logging path for respawn notices. The guard now looks at both component and message, closing the remaining known watchdog diagnostic shape without hiding real crashes.
