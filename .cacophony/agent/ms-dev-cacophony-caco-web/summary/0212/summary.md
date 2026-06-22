# Session summary — bd-b2e16c: optimistic caco-web Pico prompt echo

## Goal

Make caco-web Pico composer submission feel native and immediate. A successful prompt send should appear in the transcript right away even if the backend is slow or does not immediately echo a user-message event.

## Bead(s)

- `bd-b2e16c` — [pico] caco-web: optimistic native user bubble for sent Pico websocket prompts.

## Before state

- caco-web sent prompt HostRequests over WebSocket and cleared the textarea, but the transcript could appear unchanged until backend reconciliation.
- The mock backend proved outbound prompt receipt, but the UI did not prove immediate local feedback.

## After state

- Successful prompt sends add an optimistic `pico-user pico-pending` bubble immediately.
- Pending echoes include compact timestamp metadata and a subtle “sending” marker.
- Pending echoes are filtered out if the authoritative transcript already contains the same user message, preventing duplicate bubbles.
- Slash commands and local notes are not optimistic-echoed as user messages.
- The mock scenario asserts `pending=1` and transcript contains `hello from mock scenario` immediately after submit.
- Validation is green: caco-web-observe 12 tests; caco-web lib 644 tests; mock scenario console-clean.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-web/static/app.js` — pendingOutgoing state, optimistic echo rendering, prompt-send hook.
  - `crates/caco-web/static/style.css` — pending/sending styling.
  - `crates/caco-web/src/bin/caco-web-observe.rs` — mock scenario submit-step assertion for optimistic echo.
  - `crates/caco-web/src/tests.rs` — source guard for prompt-only optimistic echo.
  - `.cacophony/agent/.../summary/pending/web/optimistic-mock-test/` — scenario evidence.
- Tests: +1 caco-web source test for optimistic echo behavior.
- Behavioural delta: prompt sends now show immediate native user feedback while the websocket/backend catches up.

## Embedded artefacts

- `web/optimistic-mock-test/pico-optimistic-observe.log` — scenario log with pending=1 and outbound frames.
- `web/optimistic-mock-test/pico-optimistic-server.log` — dev server log.
- `web/optimistic-mock-test/screenshots/*.png` — scenario screenshots.
- `web/validation.txt` — command/results summary.

## Operator-takeaway

Pico prompt submission now feels instantaneous in the browser: the user sees their message in the native transcript immediately, while the canonical websocket prompt still goes to the backend and later authoritative transcript data de-duplicates it.
