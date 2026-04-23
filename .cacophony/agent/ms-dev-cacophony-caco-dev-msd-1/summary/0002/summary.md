# Session summary — bd-ea6668 beads-sync 500 observability

## Goal

Make the recurring `POST /beads/sync -> 500` on ms-mac daemon
diagnosable from daemon.log without needing a debugger or
out-of-band tracing. Also stop counting operationally-distinct
failure modes (transient git, lock contention, destructive-write
refusal) as generic 500s in dashboards.

## Bead(s)

- `bd-ea6668` — POST /beads/sync -> 500 recurs every ~20min on
  ms-mac daemon — no handler-side trace logged.

## Before state

- `bead_error_response()` matched only `NotFound` /
  `InvalidOperation` / `AlreadyExists` and dumped every other
  variant (Git, GitTimeout, Db, Io, SyncInProgress,
  DestructiveShrinkRefused, Other) into a generic `internal_error`
  with NO log entry.
- 11 occurrences in 3.7h on ms-mac with the http-layer line
  `POST /api/v1/projects/cacophony/beads/sync -> 500` and zero
  handler-side context in the surrounding ±30 lines of daemon.log.
- Task-join panics inside `spawn_blocking` likewise made it into the
  response body but not into daemon.log.

## After state

- New `bead_error_response_for(request_id, err, operation)` logs a
  structured `[bd-ea6668] beads-{op} {kind} request_id=... cause=...`
  line for every 5xx-producing variant.
- Status classification:
  - `BeadsError::Git` → `503 Service Unavailable`
  - `BeadsError::GitTimeout` → `504 Gateway Timeout`
  - `BeadsError::SyncInProgress` → `429 Too Many Requests`
  - `BeadsError::DestructiveShrinkRefused` → `409 Conflict`
  - `BeadsError::Db` / `Io` / `Other` → `500` with logged cause
- `handle_beads_sync` now wires the new function with `operation=
  "sync"` and also logs task-join panics with the same format.
- Existing `bead_error_response()` retained as a thin delegate so
  every other bead handler keeps its current behaviour.
- `cargo test-small` green; `cargo clippy -p caco-daemon` clean.

## Diff summary

- Commit: `ead43e03` (bd-ea6668: structured error logging + HTTP
  status classification for /beads/sync).
- Files touched: `crates/caco-daemon/src/beads.rs` (+110 / -3).
- Tests: +0 / -0 / flipped 0 (no behavioural test was previously
  pinning the all-500-no-log behaviour).
- Behavioural delta: clients hitting transient git failures now see
  503/504 and may retry; clients racing on the sync lock now see 429
  with a specific error code. Operators see the inner cause in
  daemon.log on every 5xx.

## Operator-takeaway

The 20-minute beads-sync 500 cadence is now self-diagnosing — the
next time it fires, daemon.log will carry the inner cause (git
timeout, db busy, peer fan-out failure, etc.) so root cause can be
identified without attaching a debugger. The auto-filed per-project
duplicate beads (bd-1e544d, bd-29fcf4, bd-2d2904, bd-295650,
bd-061810 etc.) should now produce actionable evidence on their next
firing and can be closed once the root cause is identified.
