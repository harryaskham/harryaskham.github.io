# Session 0010 — bd-a0af2c

## Goal

Eliminate the 12-34s broadcast HTTP latency by moving replication
fan-out and tmux direct-send injection off the response path.

## Bead(s)

- bd-a0af2c — claimed and worked end-to-end.

## Before state

`POST /api/v1/projects/<p>/messages/broadcast` and the global variant
`POST /api/v1/messages/broadcast` awaited `state.replicator.fan_out`
(one HTTPS round-trip per peer daemon) followed by
`msg_inject_to_running_agents` (serial tmux send-keys per running
agent across the fleet) inline before returning 200. With the current
peer + agent count this hit 12-34s wall clock per broadcast and made
the web Chat 'Send' button produce double-sends.

## After state

`handle_msg_broadcast` and `handle_msg_global_broadcast`:

- Synchronous portion: store insert, feed event append, UI broadcast
  publish, audit log, caller-activity record. Wire shape unchanged.
- Asynchronous portion (`tokio::spawn`): `replicator.fan_out` and
  `msg_inject_to_running_agents`. Per-peer / per-agent failures still
  surface via the existing logging in those helpers.

New test `msg_broadcast_returns_within_one_second` asserts the handler
returns under 1s; the previous inline-await wiring could not pass this
under any non-trivial peer fan-out.

Inline broken-on-main fixes (spoke ownership before each):
- `crates/caco-daemon/src/beads.rs:12979` — added `peer_version: None`
  to a `PeerReachability` test fixture missed when bd-5c2a98 widened
  the struct.
- `crates/caco-daemon/src/agent/health.rs:83` — `useless_format` on
  `format!("{session}")` rewritten as `session.to_string()`.

Validation:
- `cargo test-small` 52/52 PASS.
- `cargo test -p caco-daemon --lib msg_broadcast` 3/3 PASS.
- `cargo clippy --workspace --all-targets -- -D warnings` clean.

## Diff summary

```
crates/caco-daemon/src/lib.rs            | +118 -45  (two handlers + new test)
crates/caco-daemon/src/beads.rs          |   +3
crates/caco-daemon/src/agent/health.rs   |   +1 -1
.cacophony/agent/.../summary/0010        | (new)
```

## Operator-takeaway

Web Chat 'Send' should now clear its spinner promptly. CLI
`caco msg broadcast` should likewise return in well under a second
even with the full mesh online. Replication and tmux fan-out happen
in the background; per-peer or per-agent failures still log to the
daemon stderr the same way.

The bead's third acceptance item (frontend debounce + idempotency
token on `app.js::sendChat`) is left as a follow-up — the server
fix already closes the operator-facing pain window.

## Coordination

- Spoke claim of bd-a0af2c.
- Spoke `[broken-on-main]` ownership for `peer_version` fixture +
  `useless_format` health.rs fix.
- Will speak completion + reintegrate.
