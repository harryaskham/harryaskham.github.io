# Session summary 0022 — bd-3bcac8 slice 1: caco daemon shutdown

## Goal

Add a `caco daemon shutdown` CLI command + matching daemon endpoint
that requests a clean process exit (no respawn), as scaffolding for
future drain logic.

## Bead(s)

- `bd-3bcac8` — graceful shutdown (slice 1).

## Before state

- Only mechanism to stop the daemon was SIGTERM via `caco restart`
  or external kill. `caco restart` always respawns; there was no
  "stop and stay stopped" command, complicating safe binary swaps.

## After state

- New `POST /api/v1/daemon/shutdown` endpoint returns 200 with
  `{accepted: true, delay_ms: 500}` then schedules
  `process::exit(0)` after 500ms (giving the HTTP response time to
  flush).
- Route registered on both the main daemon router and the
  standalone variant.
- `caco daemon shutdown` CLI subcommand POSTs to the endpoint and
  prints "daemon shutdown accepted; daemon will exit in 500ms (no respawn)".

## Diff summary

- Commit: `ecebcf6d`.
- Files (2): `crates/caco-daemon/src/lib.rs`,
  `crates/caco-cli/src/lib.rs`.
- `cargo build -p caco-daemon -p caco-cli` + clippy: clean.

## Operator-takeaway

Run `caco daemon shutdown` to stop the daemon cleanly without
spawning a replacement. Useful for binary-swap upgrades:
`caco daemon shutdown && cp new-caco /usr/local/bin/caco && caco up`.

Note: this slice does **not** drain in-flight reconciles — pending
work is interrupted by the exit. Drain logic + 503/Retry-After for
new requests during the drain window are tracked in a follow-up bead.
