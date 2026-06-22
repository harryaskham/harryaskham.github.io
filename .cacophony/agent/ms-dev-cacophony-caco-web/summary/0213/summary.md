# Session summary — bd-6c2e0c: live Pico slash-command suggestions in mock scenario

## Goal

Deepen the caco-web Pico websocket integration proof by exercising the visible slash-command suggestion UI while connected to the mock `/session` backend, including both built-in and dynamic commands from the shared snapshot.

## Bead(s)

- `bd-6c2e0c` — [pico] caco-web: mock websocket scenario must exercise visible slash-command suggestions.

## Before state

- Source tests verified caco-web calls shared command-suggestion helpers.
- The live mock websocket scenario did not type into the composer suggestion UI or prove suggestions were visible during a connected session.

## After state

- The `pico-pane` scenario types `/mo` and asserts visible built-in suggestions `/model` and `/models`.
- It dispatches Tab and verifies completion extends `/mo` to `/model`.
- It types `/d` and asserts dynamic `/deploy` appears from `AgentViewSnapshot.available_commands`.
- The scenario keeps the existing inbound streaming, outbound prompt/command/ui_reply, timestamp, image, model picker, ended-state, and no-terminal assertions.
- Validation is green: caco-web-observe 12 tests; caco-web lib 644 tests; scenario console-clean.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-web/src/bin/caco-web-observe.rs` — live suggestions assertion and source guard.
  - `.cacophony/agent/.../summary/pending/web/suggestions-mock-test/` — scenario evidence.
- Tests: observe scenario guard extended for visible suggestions and completion.
- Behavioural delta: no production runtime change; mock scenario now proves connected suggestion UI behavior.

## Embedded artefacts

- `web/suggestions-mock-test/pico-suggestions-observe.log` — scenario log with builtins/dynamic completion output.
- `web/suggestions-mock-test/pico-suggestions-server.log` — dev server log.
- `web/suggestions-mock-test/screenshots/*.png` — scenario screenshots.
- `web/validation.txt` — command/results summary.

## Operator-takeaway

The Pico mock backend proof now covers not only streaming and outbound frames, but also the native command palette feel: built-in and dynamic slash suggestions appear and complete correctly while connected.
