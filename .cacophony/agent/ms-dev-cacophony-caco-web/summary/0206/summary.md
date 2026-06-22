# Session summary — bd-09efe2: caco-web Pico mock websocket scenario

## Goal

Begin the deeper caco-web Pico websocket integration work requested by Harry: prove the browser can consume a real `/session` WebSocket stream from a deterministic mock Pico backend, reduce frames through the shared PicoView path, and render a native conversation surface without falling back to terminal/PTY or DOM-only fixture injection.

## Bead(s)

- `bd-09efe2` — [pico] caco-web: mock /session websocket backend + fast streaming native AgentView observe scenario.

## Before state

- `caco-web-observe --scenario pico-pane` had a deterministic DOM fixture path, but it did not exercise an actual WebSocket session source.
- The browser had shared PicoView wasm and rich native rendering, but there was no repeatable mock-backend harness to validate fast event streaming, diff-like incremental frame handling, and no terminal fallback.

## After state

- The `pico-pane` observe scenario starts a local axum WebSocket mock backend at an OS-assigned port.
- The standalone `/pico` page is opened with `?agent=pico-observe-fixture&ws=<mock-url>`, driving the production browser WebSocket and PicoView `apply_line` path.
- The mock streams a representative HostMessage sequence: snapshot, streaming thinking/text, tool start/update/end, queue update, extension UI request, available models, get_messages with inline image, session stats, compaction activity, and AgentEnd.
- The scenario asserts native DOM state for bubbles, dialog panel, model picker, inline image, footer notice/widget, composer, suggestions host, and absence of terminal/PTY pane.
- The scenario run is console-clean: zero errors and warnings.
- Validation is green: caco-web-observe 12 tests, caco-web lib 640 tests.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-web/src/bin/caco-web-observe.rs` — mock websocket server, mock frame stream, scenario rewired to `/pico?ws=`, and source tests.
  - `.cacophony/agent/.../summary/pending/web/mock-ws-test/` — scenario logs/screenshots/snapshot.
- Tests: +3 caco-web-observe tests for pico-pane parsing, mock frames, and real-websocket scenario contract.
- Behavioural delta: no production runtime change; adds deterministic integration evidence for the Pico browser session path.

## Embedded artefacts

- `web/mock-ws-test/pico-mock-observe.log` — scenario run log and assertions.
- `web/mock-ws-test/pico-mock-server.log` — dev server log.
- `web/mock-ws-test/screenshots/page-2026-06-16T07-54-41-371Z.png` — final mock WebSocket scenario screenshot.
- `web/mock-ws-test/page-snapshots/page-2026-06-16T07-54-26-776Z.yml` — page snapshot.
- `web/validation.txt` — command/results summary.

## Operator-takeaway

caco-web now has a deterministic mock Pico `/session` backend proof: one observe scenario exercises the real browser WebSocket/PicoView path, streams representative frames quickly, validates native conversation rendering, and records console-clean screenshots.
