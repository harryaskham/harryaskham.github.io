# Session summary 0030 — bd-fd0ed4: caco msg status (slice 1)

## Goal

Give `caco msg send` callers a way to confirm whether a message
was actually received. Previously the send response only confirmed
"queued for delivery" — no way to know if the target inbox has
seen or read it.

## Bead(s)

- `bd-fd0ed4` slice 1 — read-only status query.

## Before state

- `caco msg send` returns "sent message msg-XXX to ... in project
  cacophony" but that's only *queued*, not delivered + read.
- No public endpoint to look up a single message by id.
- Operators had to scrape the recipient's inbox to find out
  whether a message landed.

## After state

- New `MessageStore::get_message(conn, id) -> Result<Option<Message>>`.
- `GET /api/v1/messages/{id}/status` returns
  `{id, project, sender, target, kind, ts, read_at, expires_at, state}`
  where `state` ∈ `{delivered, read, expired}` derived from the
  existing `read_at` / `expires_at` columns.
- `caco msg status <id>` CLI command (positional, with `--id` /
  `--message-id` aliases). Pretty-prints state, sender, target,
  ts, read_at, expires_at; `--json` returns the full envelope.

## Diff summary

- Commit: `a47e0f7f`.
- Files (3): caco-daemon messaging.rs, caco-daemon lib.rs,
  caco-cli lib.rs.
- `cargo build` and `cargo clippy` for caco-daemon + caco-cli: clean.

## Operator-takeaway

Run `caco msg status <msg-id>` after `caco msg send` to confirm
whether the message was received and read. State is derived from
existing schema — no new columns needed. The richer
state-machine (queued → delivered → read → acted-on, distinct
`delivered_at` from `read_at`, `--require-ack` send mode, `--cc`
operator forwarding, and SSE delivery events) is filed as
bd-fd0ed4 slice 2.
