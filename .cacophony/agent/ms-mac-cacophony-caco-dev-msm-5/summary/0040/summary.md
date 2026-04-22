# Session summary 0040 — bd-d5d63b: caco rehydrate (slice 1)

## Goal

Give crash-revived persistents a one-shot read-only state-hint
dump so the post-revival agent can self-rehydrate without
operator-prompt.

## Bead(s)

- `bd-d5d63b` slice 1 — read-only CLI only.

## Before state

- After a managed-session-revival-after-crash (or OOM-kill or
  upgrade restart), a persistent had no built-in way to figure
  out where it left off; operator had to hand-feed context.

## After state

- `caco rehydrate [agent-id]` (defaults to `$CACO_AGENT_ID`).
- `--project <name>` (defaults to `$CACOPHONY_PROJECT` or
  `cacophony`).
- Hits two existing daemon endpoints:
  - `GET /api/v1/projects/{project}/messages/inbox?limit=20`
  - `GET /api/v1/projects/{project}/beads?assignee={agent}&limit=5&sort=updated_at`
- Pretty-prints `## Recent inbox` (kind, sender, 120-char body
  snippet) and `## Recent beads claimed by agent` (id, status,
  title).
- `--json` mode emits structured envelope for scripting.

## Diff summary

- Commit: `ed88180a`.
- Files (1): caco-cli lib.rs (+149 lines).
- `cargo build` and `cargo clippy`: clean.

## Operator-takeaway

After a persistent restarts, the supervisor (or the agent itself
via on_revival hook) can run `caco rehydrate` to get a fresh
state-hint dump. Slice 2 (daemon-side last-N-events checkpoint
per persistent + profile `on_revival` hook that auto-runs
`caco rehydrate` post-restart) is filed as a follow-up — that
turns this manual command into automatic post-crash recovery.
