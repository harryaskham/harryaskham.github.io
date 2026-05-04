# Session summary — bd-230fa7 restart-window beads sync 500 classification

## Goal

Implement `bd-230fa7`: stop planned ms-mac restart windows from producing cross-project `/api/v1/projects/*/beads/sync -> 500` `log_error` / Errors-tab bursts when in-flight beads sync tasks are cancelled during daemon shutdown.

## Bead(s)

- `bd-230fa7` — `Multi-project beads sync HTTP 500 burst during ms-mac restart window`

## Findings

- Recent daemon log tails showed the recurring events happen inside daemon SIGTERM/restart windows.
- The key evidence line sequence was:
  - `daemon stopped: sigterm` / `daemon stopped: restart`
  - `[bd-ea6668] beads-sync task panic ... cause=task ... was cancelled`
  - `POST /api/v1/projects/<project>/beads/sync -> 500`
- Existing `bd-d81257` suppression only recognized durable planned beads-primary maintenance. It missed local SIGTERM/restart drain races where the current daemon is already shutting down and cancels in-flight sync tasks before/without the planned-outage marker being visible to the telemetry middleware.

## Changes

- Updated `crates/caco-daemon/src/lib.rs`:
  - Marks SIGTERM/restart shutdown as a local drain window by calling `state.drain.begin_drain()` before logging the stop event and returning from `run()`.
  - Extends daemon HTTP error reporting classification so `/api/v1/projects/<project>/beads/sync` 5xx responses are not mirrored into `log_error` / Errors-tab exceptions when either:
    - the active beads primary has planned maintenance, or
    - the local daemon is already draining during SIGTERM/restart.
  - Added regression test `daemon_http_error_reporting_suppresses_beads_sync_during_local_drain_bd_230fa7`.
- Updated `crates/caco-daemon/src/beads.rs`:
  - If a beads sync blocking task is cancelled while local drain is active, returns HTTP 503 `daemon_draining` instead of generic HTTP 500 `internal_error`.
  - Added `daemon_draining_response(...)` helper.
  - Added regression test `beads_sync_cancelled_during_drain_returns_503_bd_230fa7`.
- Updated `SPEC.md`:
  - Clarified that beads-sync failures during active primary planned maintenance or local SIGTERM/shutdown drain must remain caller-visible as retryable/maintenance failures, but must not be recorded as daemon-bug `log_error` exceptions; cancelled in-flight sync tasks during local drain should return 503 `daemon_draining`.

## Validation

- `rustfmt --edition 2021 --check --config skip_children=true crates/caco-daemon/src/lib.rs crates/caco-daemon/src/beads.rs` — passed.
- `git diff --check` — passed.
- `cargo check -p caco-daemon` — passed.
- `cargo clippy -p caco-daemon --lib --no-deps -- -D warnings` — passed.
- `cargo test -p caco-daemon bd_230fa7 -- --test-threads=1` — passed (2 tests).

## Notes

- I initially invoked `cargo test` with two separate test names, which Cargo rejected as a usage error. I reran the intended focused filter as `bd_230fa7`, and both regression tests passed.
- This is intentionally a narrow classification/response fix. It does not change ordinary non-sync HTTP 500 visibility, nor does it hide non-maintenance beads sync bugs.
