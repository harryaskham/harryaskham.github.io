# Session summary — bd-7d524a: outbound Pico model-picker HostRequest proof

## Goal

Extend the caco-web Pico bidirectional WebSocket proof beyond prompt/slash/dialog traffic. The mock scenario rendered the shared model picker, but it did not prove that clicking a model option sends the shared HostRequest back to the Pico backend.

## Bead(s)

- `bd-7d524a` — [pico] caco-web: mock websocket scenario must capture outbound model-picker HostRequest.

## Before state

- `pending_model_picker` rendered visually in caco-web.
- The live mock scenario asserted model picker presence, but it did not click an option or validate the outbound model-selection protocol frame.

## After state

- Model picker buttons carry a stable `.pico-model-option` class for observe/automation and future a11y probes.
- The `pico-pane` scenario clicks the visible `anthropic/claude` option through the real browser DOM.
- The mock WebSocket backend records and asserts the canonical shared frame:
  - `{"kind":"command","type":"set_model","provider":"anthropic","modelId":"claude"}`
- Existing prompt, compact, dialog reply, slash suggestions, render coalescing, image/timestamp/title/session-ended, and console-clean assertions remain intact.
- Validation is green: caco-web-observe 12 tests; caco-web lib 645 tests.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-web/static/app.js` — stable model option class.
  - `crates/caco-web/src/bin/caco-web-observe.rs` — model click step and outbound `set_model` assertion.
  - `crates/caco-web/src/tests.rs` — source guard for model-option class.
  - `.cacophony/agent/.../summary/pending/web/model-picker-test/` — scenario evidence.
- Behavioural delta: visible Pico model picker interactions are now proven bidirectional over the structured `/session` WebSocket path.

## Embedded artefacts

- `web/model-picker-test/pico-model-observe.log` — scenario log with selected model and received frames.
- `web/model-picker-test/pico-model-server.log` — dev server log.
- `web/model-picker-test/screenshots/*.png` — scenario screenshots.
- `web/validation.txt` — command/results summary.

## Operator-takeaway

caco-web Pico now proves the model picker is not just decorative: clicking a model option sends the same shared `set_model` HostRequest shape used by the Pico composer helpers.
