# Session summary — TTS/STT watchdog crash-log routing

## Goal

Stop routine TTS/STT PID-only watchdog stale-process diagnostics from polluting `daemon-crash.log`, and remove the stale/nonexistent `bd-5f8223` runtime reference from watchdog messages while preserving real crash evidence.

## Bead(s)

- `bd-1826db` — TTS/STT watchdog respawns spam daemon-crash.log with nonexistent bd-5f8223 reference

## Before state

- Failing tests: none known for this checkout before the change.
- Relevant metrics: log-monitor reported `daemon-crash.log` at 453,216 bytes with 25 timestamped lines in the 2026-05-09T01:36:12Z to 2026-05-09T01:51:12Z sweep; 24 lines were classified as watchdog stale-process diagnostics.
- Context: `reconverge_pid_only` emitted `eprintln!` watchdog warnings for TTS/STT stale process kills, and the text cited `bd-5f8223`, which the authoritative bead primary reported as not found.

## After state

- Failing tests: none from focused validation.
- Relevant metrics: focused queued validation passed before rebase (`tj-cdb3e941`, `tj-d3ed0036`, `bj-32ed9084`) and after rebase (`tj-aaa7113b`, `tj-39709b9f`, `bj-440854c0`).
- Context: stale PID-only watchdog diagnostics now append to the service log with stable `diagnostic=pid-only-watchdog` metadata, daemon watchdog respawn notices use `EventLogger::log_without_stderr_mirror`, and the stderr router treats legacy watchdog stderr as `DaemonLog` rather than crash evidence.

## Diff summary

- Commits: `d03fe27a2d`.
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`, `crates/caco-daemon/src/lib.rs`.
- Tests: +2 sidecar unit tests / -0 / flipped 0.
- Behavioural delta: TTS/STT watchdog stale-process and respawn diagnostics are preserved in normal daemon/service logging without entering `daemon-crash.log`, and new runtime diagnostics no longer mention the stale nonexistent bead ID.

## Operator-takeaway

This is another crash-log hygiene fix: the watchdog still kills and respawns stale TTS/STT processes, but those expected supervision diagnostics are no longer treated as daemon crash evidence or annotated with a dead bead reference.
