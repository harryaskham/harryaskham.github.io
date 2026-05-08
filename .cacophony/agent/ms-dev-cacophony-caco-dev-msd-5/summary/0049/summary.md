# Session summary — Guard audio 502 crash-log routing

## Goal

Close the post-close regression report that a non-fatal audio transcription HTTP 502 line still appeared in `daemon-crash.log` after the earlier daemon HTTP logging fix. The goal was to make sure the exact observed line is covered by regression tests and cannot be routed to the crash log by the current stderr router.

## Bead(s)

- `bd-45b572` — Audio transcription 502 still writes to daemon-crash.log after bd-e2b586 closed

## Before state

- Log-monitor evidence showed `2026-05-08T18:03:31.489Z ERROR [daemon:http] [project:cacophony] POST /api/v1/audio/transcription -> 502 ... "fatal":false ...` in `daemon-crash.log` on ms-mac.
- `bd-e2b586` had already stopped non-fatal `daemon:http` structured errors from directly mirroring to stderr.
- `bd-e5e022` had added a sidecar stderr router so already-logged nonfatal daemon stderr lines are dropped instead of appended to `daemon-crash.log`; this bead needed explicit coverage for the audio transcription 502 recurrence shape.

## After state

- Added a focused sidecar regression test using the exact observed non-fatal `daemon:http` audio transcription 502 line.
- The test asserts `daemon_stderr_route_for_line` classifies that line as `AlreadyLogged`, keeping it out of `daemon-crash.log` while preserving daemon.log/feed/Errors diagnostics.
- Focused queued validation passed: `tj-0484b4ba` ran `cargo test -p caco-sidecar daemon_stderr_router_drops_nonfatal_daemon_http_502_bd_45b572 -- --nocapture` successfully.

## Diff summary

- Commits: `728dea195e`
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`
- Tests: +1 focused regression test.
- Behavioural delta: no new runtime branch was needed beyond the already-landed stderr router; this bead pins the reported audio 502 recurrence to that routing contract so future changes cannot regress it silently.

## Operator-takeaway

The observed post-close audio 502 crash-log line is now an explicit sidecar routing regression fixture: with the current code path, that non-fatal `daemon:http` line is treated as already durable diagnostics and is not crash evidence.
