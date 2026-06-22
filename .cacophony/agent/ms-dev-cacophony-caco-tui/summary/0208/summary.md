# Session summary — /loop list local introspection (bd-92b99b)

## Goal

Foundation slice of the TUI loop-management system (parent bd-08e448, decomposed
by msm-1): give a managed Pi agent a way to introspect its OWN active loops. Add
a `/loop list` subcommand to the repo-owned `.cacophony/pi/loop/` overlay that
shows each active loop's id, interval, next-fire, and prompt. Self-contained in
the loop overlay (no CLI/mesh); the metadata this exposes is what the later
`caco loop list` aggregation slice builds on.

## Bead(s)

- `bd-92b99b` — Loop-mgmt slice A: local /loop introspection (expose an agent's
  own active loops). [claimed + landed this session]
- parent: `bd-08e448` (TUI loop-management system).

## Before state

- `.cacophony/pi/loop/extensions/caco-loop.mjs` supported `/loop <interval>
  <prompt>` / `/loop status` / `/loop off`, with a single `activeLoop` and no
  list/introspection view and no stored next-fire timestamp.
- Overlay tests: 82 passing (whole `node --test` overlay glob).

## After state

- `/loop list` added: lists the agent's active loop(s) as
  `- <id> | every <interval> | next <countdown> | <delivery> while busy |
  <prompt>`, or "No active loops" when none. Stable per-loop id derived from the
  persisted `startedAt` (survives session restore, no new persisted field);
  `nextFireAt` tracked in `scheduleNextTick`. Completions + usage text updated to
  include `list`. Structured as a list (currently one loop) for the bd-08e448
  aggregation slice.
- Overlay tests: 82 -> 82 still passing, +1 new test (`bd-92b99b`) for the
  no-loop and active-loop cases. `node --test` full overlay glob: 82 pass, 0 fail.

## Diff summary

- Code commit: 7889af168c (final landed squash SHA from the reintegration
  receipt). Summary artefact commit: intentionally omitted.
- Files: `.cacophony/pi/loop/extensions/caco-loop.mjs` (list subcommand +
  helpers + nextFireAt + usage/completions), `caco-loop.test.mjs` (+1 test).
- Tests: +1; whole overlay glob 82 pass / 0 fail.
- Behavioural delta: new read-only `/loop list` introspection command; no change
  to scheduling/delivery/persistence behaviour.
- Validation: JS overlay tests via `node --test` (the CI `npm test` glob) — the
  reintegration cargo gate doesn't cover JS, so this is the authoritative check
  for this overlay change.

## Operator-takeaway

Managed Pi agents can now run `/loop list` to see their own scheduled loops with
next-fire countdowns — the read-only foundation the fleet-wide loop-management
surface (bd-08e448: `caco loop list` aggregation, control) builds on. It's
intentionally structured as a list even though the overlay currently runs one
loop at a time.
