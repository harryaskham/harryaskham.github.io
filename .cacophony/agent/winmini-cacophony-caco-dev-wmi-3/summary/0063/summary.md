# Session summary — pico chat duplicate user messages (bd-d83671)

## Goal

Fix the caco-web browser dashboard bug where sending a message in a pico
session chat displays the user message twice, both stuck on "sending…". The
goal was a contained, low-risk frontend fix in the pico composer/optimistic-echo
path that makes a sent message appear exactly once, without disturbing the
streaming render path (owned in parallel by wmi-2 on bd-87b72d) or the
intentional retry-by-resend multi-bubble behaviour.

## Bead(s)

- `bd-d83671` — Fix duplicate user messages in pico session chat UI (P1 bug,
  labels chat-ui/messaging/pico-sessions)
- Reflection draft filed: `bd-07b0d3` — caco-web: no lightweight JS unit-test
  lane for pure static/app.js helpers (dedup/guard logic only browser-testable)

## Before state

- Failing tests: none known (the bug is a frontend rendering/dedup defect not
  covered by any Rust workspace test; caco-web static JS has no unit-test lane).
- Symptom: a user prompt rendered as two bubbles, both "sending…".
- Root cause (confirmed with the caco-web/pico specialist + aurora pico-spike
  lead aur-2): two optimistic render sources in `crates/caco-web/static/app.js`
  — the JS `picoState.pendingOutgoing` echo (`renderPicoPendingOutgoing`) and
  the daemon-echoed transcript User item's `transcript_send_states` Pending
  badge (`picoSendStateExtra`). `renderPicoPendingOutgoing` deduped by EXACT
  text; `sendPicoPrompt` stored the RAW typed `input.value` in
  `pendingOutgoing` while it sent the composed/normalized body
  (`picoComposerLine` → `action.text`, typically trimmed by `parseComposer`).
  Any trim/normalization difference between the raw text and the daemon-echoed
  transcript text made the dedup miss → both bubbles persisted. There was also
  no duplicate-submit guard, so a mobile `enterkeyhint="send"` double-fire
  (form onsubmit + keydown-Enter) could send the prompt to the server twice.

## After state

- Failing tests: none (JS `node --check` clean on app.js; linux reintegration
  gate cargo check/test-small/clippy green on the prospective merge commit).
- A sent prompt renders exactly one optimistic bubble that reconciles cleanly
  once the daemon echoes it back; duplicate optimistic entries collapse; an
  accidental same-prompt double-fire within 1.5s is dropped so the server
  receives the prompt once.
- Retry-by-resend (cacoPicoRetrySend) multi-bubble behaviour and command/note
  composer paths are unchanged (the guard + optimistic echo apply to prompt
  kind only).

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-web/static/app.js` (single file, +51 / -6).
- Changes:
  - `picoComposerLine` now returns the `body` actually sent so the optimistic
    echo + dedup key match the daemon-echoed transcript User text.
  - `sendPicoPrompt` stores that composed body (not raw input) in
    `pendingOutgoing`, and drops an identical prompt resubmit within
    `PICO_DUPLICATE_SUBMIT_WINDOW_MS` (1500ms) via a new
    `picoState.lastPromptSubmit` key/ts (prompt kind only).
  - `renderPicoPendingOutgoing` dedups against transcript User items using
    TRIMMED text and collapses duplicate-text pending entries into one bubble.
  - Added `PICO_DUPLICATE_SUBMIT_WINDOW_MS`, `picoState.lastPromptSubmit`, and a
    teardown reset of that field.
- Tests: +0 / -0 / flipped 0 (no JS unit-test lane exists; see draft bd-07b0d3).
- Behavioural delta: pico user messages appear exactly once; mobile double-fire
  no longer double-sends; retry/command/note paths untouched.

## Operator-takeaway

The pico chat "duplicate sending" bug was a dedup-key mismatch, not a hard
double-submit: the optimistic echo keyed on the raw typed text while the daemon
echoed the normalized body, so exact-match dedup missed. Storing the
actually-sent body + trim-tolerant dedup is the real fix; the 1.5s submit guard
is defense-in-depth for the mobile enterkeyhint double-fire. Validation was
code-reasoning + JS syntax + the linux cargo gate only — winmini can't run the
pico web UI, so a visual confirm from a caco-web/pico specialist (offered by
caco-web-msd-0 / aur-2) is the recommended post-land check. The deeper gap
(no headless unit-test lane for pure caco-web JS helpers) is filed as bd-07b0d3.
