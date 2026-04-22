# Session summary — bd-e33b44 caco msg snapshot

## Goal

Land bd-e33b44, the small bd-d5d63b sibling that adds
`caco msg snapshot --agent <id> --last N --json` — a
read-only structured dump of the last-N inbox messages
addressed to an agent, suitable for piping into a revival
agent's first turn (or an on_revival hook from bd-1ac1b7).

## Bead(s)

- `bd-e33b44` — [bd-d5d63b follow-up] `caco msg snapshot`:
  structured last-N inbox dump for rehydration prompts.

## Diff summary

**`crates/caco-cli/src/lib.rs`:**

- New `MSG_SNAPSHOT_ARGS` (`--project` required, `--agent`
  required, `--last` default 10, `--include-system`).
- New `msg snapshot` entry in `MSG_SUBCOMMANDS` —
  `mcp_enabled = true`, `agent_safe = true`,
  `idempotent = true` (read-only).
- New `dispatch_msg_snapshot` (~100 lines): validates
  `--last > 0`, calls existing
  `/api/v1/projects/{project}/messages/inbox?limit={n}`,
  filters client-side by `target` substring containing the
  agent ID (matches `caco msg inbox --target` semantics),
  emits stable `{ok, data: {project, agent, requested,
  returned, include_system, messages: [{id, ts, sender,
  target, kind, body}]}}` JSON envelope. Text mode emits
  one line per message with body truncated at 140 chars.
- New `msg_snapshot_subcommand_exposed_in_spec` test
  asserting MCP / agent_safe / idempotent flags and the
  required-vs-optional arg matrix.

## Before state

- The "last 10 messages" bullet from bd-d5d63b's
  rehydration spec required the consumer to call
  `caco msg inbox --target <agent> --tail 10 --json`
  themselves and post-process — works but every consumer
  reinvents the same envelope shape.
- bd-d5d63b slice 1 (`caco rehydrate`) hasn't landed yet;
  this bead provides a primitive both that command and
  bd-1ac1b7's `on_revival` hook can call.

## After state

- `caco msg snapshot --project P --agent A --last 10` is
  the one-shot way to get a rehydration-ready inbox tail.
- JSON envelope is stable and documented in the dispatcher
  docstring.
- `cargo test -p caco-cli --lib msg_snapshot` 1/1 green.
- `cargo build -p caco-cli` green.

## Notes / verification

- `cargo test-small` shows 1 pre-existing failure in
  `caco-beads` (bd-10e37c — `reconcile_skips_export_when_content_unchanged`
  fails on origin/main after bd-fb9318's
  `estimated_effort` schema add; assigned to msm-3, not
  this-cycle regression).
- Implementation reuses `apply_provenance` /
  `async_send_request` and the existing inbox endpoint —
  no daemon-side change required.
- Client-side substring filter on `target` matches the
  semantics of `caco msg inbox --target`, so callers don't
  need to think about full caller-id triples.

## Out of scope

- `caco rehydrate` itself (bd-d5d63b slice 1) — different
  agent owns it.
- `on_revival` hook + daemon-side checkpoint (bd-1ac1b7) —
  that's the multi-step follow-up.
- A daemon-side `?target=<agent>` filter on the inbox
  endpoint — would be a nice optimization but not needed
  while the snapshot is bounded to last-N.

## Operator-takeaway

`caco msg snapshot --project P --agent A --last N` returns
a stable JSON envelope of the last N inbox messages for
agent A. Designed to feed the bd-d5d63b rehydration prompt
flow (and bd-1ac1b7's `on_revival` hook) without each
consumer re-implementing the envelope shape.
