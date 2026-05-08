# Session summary — Keep daemon HTTP errors out of crash log

## Goal

Stop routine non-fatal daemon HTTP route failures, specifically audio transcription 502 responses, from being mirrored into `daemon-crash.log` while preserving their existing structured diagnostics in daemon logs, feed events, and the Errors surface.

## Bead(s)

- `bd-e2b586` — Non-fatal audio transcription 502 responses are written to daemon-crash.log

## Before state

- Log-monitor observed repeated `POST /api/v1/audio/transcription -> 502` lines in `~/.cacophony/daemon/daemon-crash.log` while `caco status` and `caco service status` remained healthy.
- The request telemetry path reported daemon HTTP 5xxs as structured `log_error` records with component `daemon:http` and severity `error`.
- Structured `error` records were mirrored through `EventLogger::log`, which writes non-debug entries to stderr; supervised deployments capture that stderr into `daemon-crash.log`.

## After state

- Non-fatal structured log errors with component `daemon:http` are still persisted to daemon.log, feed, exception/Error surfaces, and replication, but are logged through `log_without_stderr_mirror` so they do not enter daemon stderr / `daemon-crash.log`.
- Fatal daemon HTTP records still mirror to stderr, and non-HTTP non-warning errors keep existing stderr visibility.
- SPEC, README, and AGENTS now document that non-fatal daemon HTTP request diagnostics must not be mirrored into crash logs.
- Focused validation passed: `tj-35312c51` ran `cargo test -p caco-daemon daemon_http_log_errors_do_not_mirror_to_stderr_bd_e2b586 -- --nocapture` successfully.

## Diff summary

- Commits: `b26f970d91`
- Files touched: `crates/caco-daemon/src/lib.rs`, `SPEC.md`, `README.md`, `AGENTS.md`
- Tests: +1 focused daemon test for the stderr-mirroring policy.
- Behavioural delta: routine daemon HTTP 5xx diagnostics remain operator-visible through structured observability but no longer grow `daemon-crash.log` unless explicitly fatal.

## Operator-takeaway

The fix narrows only stderr mirroring, not error reporting: audio transcription 502s and similar non-fatal route errors stay diagnosable in the right surfaces without polluting the crash log that operators use for real daemon crash evidence.
