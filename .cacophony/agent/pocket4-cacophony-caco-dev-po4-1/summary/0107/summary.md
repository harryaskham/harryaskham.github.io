# Session summary — bd-7e5dfe foundation (group-chat TTS fan-out dedupe)

## Goal

Stop a single group/members-only chat message being TTS-spoken once per
recipient. A group message fans out (client-side) into N per-recipient direct
replicas, and the TTS daemon's agent-DM read-aloud spoke each replica — so one
logical message was narrated N times. This session lands the ctrl-approved,
additive, zero-regression backend foundation (correlation-id source-stamp) so a
later one-line client change can flip the dedupe on.

## Bead(s)

- `bd-7e5dfe` — Group-chat message fan-out TTS-speaks once per recipient replica
  (duplicate speech). Stays OPEN after this land for the client-activation slice.

## Before state

- Failing tests: none (the bug is duplicate narration, not a red test).
- TTS daemon `message_sent` arm derived `speak_id` from the per-replica
  `message_id` (distinct per recipient) → the dedupe ledger could not collapse
  fan-out replicas → N speaks. No shared origin id existed on the replicas.
- 7 prior agents investigated + unclaimed (multi-component, narration-sensitive);
  a content-keyed (sender+body) tight-window heuristic was explicitly REJECTED by
  ctrl (it silently drops legitimate identical messages). wmi-2 pinned every exact
  site and the safe decomposition.

## After state

- Failing tests: none. `cargo check --workspace --tests` green (tj-85e5da6a);
  4 new correlation tests pass (tj-8ef7bd59).
- Backend foundation wired end to end, additive (no behavior change until a
  client sends `correlation_id`): CLI `--correlation-id` flag → daemon
  `SendMessageRequest`/`DirectMessageDelivery` → MessageSent feed payload
  (`with_correlation_id`) → TTS `make_tts_speak_id_with_correlation` keys the
  speak_id on the shared origin id (prefix `corr:`), collapsing replicas while
  distinct messages — even byte-identical ones — still each speak.

## Diff summary

- Code/content commit: 96bce3908 (final landed squash SHA from reintegration receipt).
- Summary artefact commit: intentionally omitted (no self-reference).
- Files touched: crates/caco-cli/src/{lib.rs,msg_cmd.rs,tts_daemon.rs},
  crates/caco-daemon/src/{lib.rs,choices.rs} (+215/-5).
- Tests: +4 (body omit/include correlation_id; speak-id helper origin-not-content;
  end-to-end message_sent fan-out collapse vs distinct-messages-still-speak).
- Behavioural delta: none yet (additive) — the dedupe activates only once clients
  stamp a shared correlation_id.

## Operator-takeaway

The dedupe is keyed on the message's ORIGIN id, never its content — so it can
NEVER silently drop a legitimately-distinct identical message (the failure mode
that got every prior heuristic vetoed). What remains to actually fix the
user-visible bug is the small client activation: the caco-web group-send loop
(static/workspace-chat-pane.js) and the caco-tui group-chat fan-out must generate
ONE `correlation_id` per group message and pass it on each of the N sends. The
bead stays open for that focused slice.
