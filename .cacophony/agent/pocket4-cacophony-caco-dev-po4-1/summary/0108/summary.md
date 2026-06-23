# Session summary — bd-7e5dfe client activation (group-chat TTS dedupe complete)

## Goal

Complete the group-chat duplicate-TTS fix: a single group/members-only message
was TTS-spoken once per recipient. This slice flips the dedupe on by having both
group-chat clients stamp one shared correlation id across their fan-out, on top
of the correlation-id backend foundation landed earlier this session.

## Bead(s)

- `bd-7e5dfe` — Group-chat message fan-out TTS-speaks once per recipient replica
  (duplicate speech). Fully fixed by this slice; closing.

## Before state

- Foundation landed (180bfaba2, durable on true GitHub): CLI `--correlation-id`,
  daemon propagation into the MessageSent payload, and TTS speak-id keyed on the
  shared id — but additive/inert because no client sent a correlation id yet, so
  group chat still spoke once per recipient.

## After state

- Failing tests: none. `cargo check --workspace --tests` green (tj-4378b11b);
  caco-tui `direct_message_payload` tests 4/4 (tj-c302e368); `node --check` on the
  caco-web JS passes.
- Both group-chat clients now stamp ONE correlation id per logical group message:
  caco-tui `App::submit_chat_message` group fan-out + caco-web
  `workspace-chat-pane.js` group mode. The daemon stamps every replica, the TTS
  daemon collapses them to one playback (origin-keyed, never content-keyed, so
  genuinely-distinct identical messages still each speak).

## Diff summary

- Code/content commits: 180bfaba2 (foundation, already landed), 1341fb972 (this
  client activation; final landed squash SHA from the reintegration receipt).
- Summary artefact commit: intentionally omitted (no self-reference).
- Files touched (this slice): crates/caco-tui/src/{app.rs,client.rs},
  crates/caco-web/static/workspace-chat-pane.js (+68/-11).
- Tests: +2 caco-tui payload tests (the foundation added +4 caco-cli tests).
- Behavioural delta: a group/members-only chat message is now TTS-spoken ONCE,
  not once per recipient.

## Operator-takeaway

The whole fix keys the TTS dedupe on the message's ORIGIN id (a per-group-message
correlation id), never on content — so it can never silently drop a legitimately
distinct identical message, which is exactly the failure mode that got every
prior content-keyed heuristic vetoed by ctrl. The id is generated client-side per
group send and flows CLI/daemon/TTS through the foundation. Ordinary direct
messages and explicit `caco msg speak` are untouched.
