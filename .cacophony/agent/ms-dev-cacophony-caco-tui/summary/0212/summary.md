# Session summary — Loop-mgmt B-i: overlay publishes loop state to fleet scratch registry

## Goal

Move the stuck loop-management fleet-visibility feature (bd-9eabbe) forward by
implementing its producer half. Two caco-tui vets had decomposed bd-9eabbe into
B-i (overlay publishes active-loop state to a fleet-readable channel) + B-ii (a
`caco loop list` CLI aggregator), coupled so B-ii returns empty without B-i. As
the de-facto loop-overlay owner (landed slices A bd-92b99b + C bd-740fe6), I took
B-i: the JS overlay now publishes each agent's active-loop state to a per-agent
scratch registry note, unblocking the fleet aggregator.

## Bead(s)

- `bd-cf1d5e` — Loop-mgmt B-i: Pi loop overlay publishes active-loop state to
  fleet scratch registry (producer for `caco loop list`).
- Parent feature: `bd-9eabbe` (slice B); epic `bd-08e448`. Depends on slice A
  (`bd-92b99b`, landed).

## Before state

- Failing tests: none. Overlay glob: 83 pass / 0 fail.
- The Pi loop overlay kept loop state only in local Pi session state + a local
  status line (`/loop list`, slice A). No mechanism existed for the overlay to
  surface loop state to a daemon/fleet-readable channel, so a fleet
  `caco loop list` had nothing to aggregate.

## After state

- Failing tests: none. Overlay glob: 85 pass / 0 fail (+2 B-i tests).
- On loop create, `/loop off`, and `session_start` restore (active loops only),
  the overlay publishes a registry record to scratch note `loops:<agent-id>` via
  `caco scratch write`. Record shape: agentId, project, node, active, loopId,
  intervalSecs, deliveryMode, nextFireAt, startedAt, lastTriggeredAt,
  promptSummary, updatedAt. Channel = scratch-registry (the bd-9eabbe vet's
  lower-risk recommendation; no new daemon feed-event type / extensible-enum
  match site). Writer is injectable via globalThis.__pi_loop_registry_writer
  (mirrors the existing globalThis.__pi_loop_api pattern); best-effort and
  non-fatal so a registry failure never breaks the Pi session.

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched: `.cacophony/pi/loop/extensions/caco-loop.mjs` (registry
  helpers + 3 hook calls + `node:child_process` import),
  `.cacophony/pi/loop/extensions/caco-loop.test.mjs` (+2 tests).
- Tests: +2 (publish-on-create + clear-on-off shape assertions; restore
  re-publishes active but not inactive). JS-only; validated via
  `node --test` (full overlay glob 85 pass / 0 fail). No cargo/nix.
- Behavioural delta: managed Pi agents now publish their active-loop state to a
  fleet-readable scratch registry; no change to loop scheduling/delivery.

## Operator-takeaway

bd-9eabbe's fleet `caco loop list` is now unblocked: each agent's active-loop
state lands in scratch note `loops:<agent-id>` (JSON record, latest-per-agent).
The remaining half (B-ii) is a read-only Rust CLI — restructure `caco loop` into
a subcommand group and add `caco loop list` that reads `caco scratch list` notes
with id prefix `loops:`, filtered by --project/--node/--id. That Rust-CLI work is
best done after the ms-dev nix host-cleanup fully lands (clean queued cargo
validation). The scratch-registry channel was chosen over a feed-event to avoid a
new daemon-Rust extensible-enum match surface.
