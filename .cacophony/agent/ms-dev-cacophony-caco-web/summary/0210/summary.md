# Session summary — bd-8e38ae: caco-web Pico mock captures outbound composer prompt

## Goal

Continue the deep Pico websocket integration work by proving not only that caco-web receives and renders mock `/session` frames, but also that the native browser composer sends canonical HostRequest prompt frames back over the same WebSocket.

## Bead(s)

- `bd-8e38ae` — [pico] caco-web: mock websocket scenario must capture outbound composer HostRequest prompt.

## Before state

- `caco-web-observe --scenario pico-pane` proved inbound streaming from a mock backend to the browser.
- It did not prove outbound traffic from the browser composer reached the backend or had the expected HostRequest shape.

## After state

- The mock Pico server records inbound WebSocket text messages.
- The scenario submits `hello from mock scenario` through the real caco-web Pico composer while the mock session is connected.
- The Rust-side scenario assertion verifies the backend receives canonical JSON: `{"kind":"command","type":"prompt","message":"hello from mock scenario"}`.
- Existing inbound render assertions remain intact: no terminal fallback, bubbles, dialog, model picker, inline image, notification/widget chips, timestamps, and session-ended status.
- Validation is green: caco-web-observe 12 tests; caco-web lib 643 tests; scenario console-clean.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-web/src/bin/caco-web-observe.rs` — inbound message capture, composer submit step, outbound HostRequest assertion.
  - `.cacophony/agent/.../summary/pending/web/outbound-mock-test/` — scenario evidence.
- Tests: caco-web-observe source tests extended to require real composer submission path and outbound assertion.
- Behavioural delta: no production runtime change; adds deterministic bidirectional websocket proof for caco-web Pico sessions.

## Embedded artefacts

- `web/outbound-mock-test/pico-outbound-observe.log` — scenario log with received prompt frame.
- `web/outbound-mock-test/pico-outbound-server.log` — dev server log.
- `web/outbound-mock-test/screenshots/*.png` — scenario screenshots.
- `web/validation.txt` — command/results summary.

## Operator-takeaway

The mock Pico harness now proves the full bidirectional path: streamed HostMessages render natively, and composer submits leave the browser as canonical HostRequest prompt frames over WebSocket.
