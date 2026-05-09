# Session summary — Prune non-crash diagnostics from crash-log rotation

## Goal

Stop known non-crash diagnostics that already reached `daemon-crash.log` from repeatedly triggering follow-up beads after the source-side guards land, by making the crash-log rotation path prune known operational noise while preserving real panic/fatal evidence.

## Bead(s)

- `bd-c381f2` — TTS/STT watchdog respawns recur in daemon-crash.log after bd-adb882 close

## Before state

- Failing tests: none known before this slice.
- Relevant metrics: log-monitor reported `daemon-crash.log` at 1,090,212 bytes with 29 timestamped lines in the 2026-05-09T03:06:10Z to 2026-05-09T03:21:10Z sweep; 14 were watchdog stale-process diagnostics.
- Context: source-side guards for idle advisories, watchdog diagnostics, and audio transcription 502s were landing, but stale lines already in `daemon-crash.log` remained in the file and kept being observed until rotation/cleanup removed them.

## After state

- Failing tests: none from focused validation.
- Relevant metrics: queued validation passed after rebase: `tj-494b17b3` for `rotate_crash_log_prunes_known_non_crash_diagnostics_bd_c381f2` and `bj-991e27f2` for `cargo check -p caco-daemon --lib`.
- Context: crash-log rotation now runs even below the byte cap when it can compact known non-crash diagnostics. It filters INFO/WARN structured lines and operational-noise signatures such as persistent idle advisories, TTS/STT watchdog diagnostics, and nonfatal audio transcription 502s, while retaining panic/backtrace evidence.

## Diff summary

- Commits: `bc178996cf`.
- Files touched: `crates/caco-daemon/src/crash_log.rs`.
- Tests: +1 daemon unit test / -0 / flipped 0.
- Behavioural delta: stale non-crash diagnostics already present in `daemon-crash.log` decay through first-party rotation instead of requiring manual file cleanup or repeatedly being re-reported as crash-log contamination.

## Operator-takeaway

The recurring watchdog beads were increasingly about old non-crash lines still resident in `daemon-crash.log`, not only fresh writes. This slice makes the daemon’s own rotation path clean those known-safe diagnostics while leaving real crash signatures intact.
