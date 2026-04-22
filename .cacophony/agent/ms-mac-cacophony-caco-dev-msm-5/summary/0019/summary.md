# Session summary 0019 — bd-ea6668: log /beads/sync 500 cause inline

## Goal

The periodic `POST /beads/sync` 500 on ms-mac (every ~20min, see
log-monitor evidence in the bead) had no handler-side trace. The
ErrorEnvelope JSON response went over the wire fine but the daemon
log only recorded the access-log status code. Add structured
inline logging so the cause is greppable in `daemon.log`.

## Bead(s)

- `bd-ea6668` slice 1 (acceptance #1) — log structured cause inline.

## Before state

- `handle_beads_sync` matched the spawn_blocking result and called
  `bead_error_response` (Ok(Err)) or `internal_error` (Err panic)
  but neither path emitted a structured log line.
- The 500 looked identical in `daemon.log` to every other
  `internal_error` 500 — root-cause invisibility.

## After state

- Both `Ok(Err)` and `Err` arms now emit `eprintln!` with:
  - `bd-ea6668` marker (greppable + bead-traceable).
  - Project name.
  - Request ID (cross-references access log).
  - `{e:?}` debug-format of the BeadsError or join error cause.
- Response semantics unchanged — JSON envelope still goes to client
  with the same status code mapping. Log additions are pure
  observability.

## Diff summary

- Commit: `587555c5`.
- Files: `crates/caco-daemon/src/beads.rs` (+15 / -2: 2 eprintln
  lines added at the result-match arms).
- Tests: none added (logging-only; existing integration tests
  exercise both code paths).
- `cargo build -p caco-daemon` and `cargo clippy -p caco-daemon`:
  clean.

## Out of scope (deferred)

- **Acceptance #2** (specific status codes — 409 conflict / 503
  transient downstream — or fixing the underlying flake): needs
  the cause-string field data first to know which flake variant
  is firing. Once tomorrow's `daemon.log` contains a sample of
  cause strings, the right specific-status mapping will be obvious.
- **Acceptance #3** (closing auto-filed sibling draft beads
  bd-1e544d et al): operator policy decision; will land once the
  underlying fix from acceptance #2 makes the recurrence stop.

## Operator-takeaway

Next /beads/sync 500 on ms-mac (or any node) will leave a
`bd-ea6668: ERROR /beads/sync project=cacophony request_id=...
cause=<BeadsError variant>` line in daemon.log. Grep for
`bd-ea6668` to pull the latest cause and triage the underlying
flake.
