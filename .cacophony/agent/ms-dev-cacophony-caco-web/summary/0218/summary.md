# Session summary — bd-a9b295: outbound Pico abort HostRequest proof

## Goal

Complete another bidirectional control in the caco-web Pico mock `/session` scenario: prove `/abort` leaves the browser as the canonical abort HostRequest while a turn is live.

## Bead(s)

- `bd-a9b295` — [pico] caco-web: mock websocket scenario must capture outbound abort HostRequest.

## Before state

- The scenario proved steer, dialog reply, model picker, normal prompt, and compact command outbound traffic.
- Submitting `/abort` while streaming incorrectly flowed through steer routing and produced `{"type":"steer","message":"/abort"}`.

## After state

- `picoComposerLine` special-cases exact `/abort` through the shared wasm `abortLine()` helper before prompt/steer selection.
- The `pico-pane` mock scenario submits `/abort` through the real composer during the explicit streaming window.
- The mock backend asserts the canonical abort frame:
  - `{"kind":"command","type":"abort"}`
- The same scenario still asserts steer, ui_reply, set_model, prompt, compact, slash suggestions, render coalescing, image/timestamp/title/session-ended, and console-clean native UI behavior.
- Validation is green: caco-web-observe 12 tests; caco-web lib 645 tests.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-web/static/app.js` — `/abort` composer special-case via shared `abortLine()`.
  - `crates/caco-web/src/bin/caco-web-observe.rs` — abort submit step and outbound assertion.
  - `crates/caco-web/src/tests.rs` — source guard for abort routing.
  - `.cacophony/agent/.../summary/pending/web/abort-command-test/` — scenario evidence.
- Behavioural delta: exact `/abort` in caco-web Pico now sends an abort command even while the stream is active, instead of becoming a steer message.

## Embedded artefacts

- `web/abort-command-test/pico-abort-observe.log` — scenario log with received abort/steer/ui_reply/set_model/prompt/compact frames.
- `web/abort-command-test/pico-abort-server.log` — dev server log.
- `web/abort-command-test/screenshots/*.png` — scenario screenshots.
- `web/validation.txt` — command/results summary.

## Operator-takeaway

caco-web Pico now proves the core live-control set over structured WebSocket: steer, abort, dialog reply, model selection, prompt, and compact all leave the browser as canonical HostRequest frames.
