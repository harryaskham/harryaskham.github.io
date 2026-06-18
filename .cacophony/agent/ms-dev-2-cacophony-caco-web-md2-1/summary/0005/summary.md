# Session summary — caco-web Chat: stop rendering bodyless speechEvents as "unknown" rows

## Goal

Fix a P2 operator-trust defect in the caco-web Chat view: speak/audio
`speechEvents` were merged into the chat list as separate rows with sender
"unknown" and an empty body (live: 126 such rows in a 252-message view),
cluttering the primary chat surface and obscuring which agent actually spoke.

## Bead(s)

- `bd-dbbedc` — caco-web: Chat renders speechEvents as 'unknown'-sender empty-body rows (uses payload.sender not top-level sender; duplicates real speak messages)

## Before state

- Failing tests: none.
- `renderChat()` (`app.js` ~6627) mapped `state.speechEvents` into the chat list
  with `sender: e.payload?.sender`. But speechEvents are audio-generation
  telemetry: the populated sender is **top-level** `e.sender`, while
  `e.payload` has only audio metadata (format/model/voice/size) and **no
  body/text**. So each speechEvent became a row with sender → "unknown"
  (fallback) and an empty body, not deduped against the real `message_speak`
  chatMessages (which carry body+sender and render correctly). Live probe: 126
  "unknown" + empty-body rows.

## After state

- Failing tests: none. `node --check` clean; `cargo test -p caco-web --lib`
  passed via the daemon test queue (exit 0). No test asserted the speechEvents
  mapping (verified before editing).
- speechEvents are now (1) attributed to their real top-level `e.sender`, and
  (2) merged into chat only when they actually carry a body
  (`payload.body|text|message`), so bodyless audio-telemetry events no longer
  render as empty rows. Live verification on the Chat view: `unknownSender: 0`,
  `emptyBody: 0`, `unknownEmpty: 0` (was 126); 56 real speak messages still
  render with correct senders/bodies; console clean. The speaking-clock speak
  ("It is 03:00 on Thursday…") still renders — it arrives as a bodied
  `message_speak`, so the bodyless filter preserves it.

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-web/static/app.js` — renderChat speechEvents merge:
  filter to body-bearing events + `sender: e.sender || e.payload?.sender`.
- Tests: +0 / -0 (no test covered this mapping). Full caco-web lib suite green.
- Behavioural delta: the Chat surface no longer shows bodyless "unknown" speak
  rows; real speak messages (incl. speaking-clock) remain, correctly attributed.

## Embedded artefacts

- `web/screenshots/after-chat.png` — Chat view after the fix: every row has a
  resolved sender + body; no empty "unknown" rows.

## Operator-takeaway

speak/audio telemetry was leaking into the primary Chat surface as 126 empty
"unknown" rows — the real spoken content was always in the `message_speak`
chatMessages. Filtering speechEvents to body-bearing events (and attributing
them to the real top-level sender) removes the clutter without losing any
readable speak, including bodied cron speaks like the speaking-clock.
