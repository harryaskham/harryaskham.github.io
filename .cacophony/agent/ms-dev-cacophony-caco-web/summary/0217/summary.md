# Session summary — bd-731b31: outbound Pico steer while streaming

## Goal

Prove caco-web Pico routes plain composer text through the shared `steer` HostRequest while a turn is actively streaming, not just as a normal prompt after the session settles.

## Bead(s)

- `bd-731b31` — [pico] caco-web: mock websocket scenario must capture outbound steer while streaming.

## Before state

- `picoComposerLine` intended to use shared `steerLine` when `snapshot.is_streaming` was true.
- The mock scenario only submitted a normal prompt after the session settled.
- During investigation, visible streaming content could exist even when the `is_streaming` flag alone was insufficient for robust composer routing.

## After state

- Added `picoIsActivelyStreaming()` to treat `is_streaming`, `streaming_text`, `streaming_thinking`, or non-empty `streaming_blocks` as active stream state.
- `picoComposerLine` uses `picoIsActivelyStreaming()` before choosing shared `steerLine` vs `promptLine`.
- Mock backend starts with an explicit streaming snapshot so the scenario has a deterministic steer window.
- Mock stream delays `pi_exited` so the scenario can exercise steer, dialog reply, model picker, normal prompt, and slash command before the final ended-state assertion.
- The scenario now receives all five outbound frames: steer, ui_reply, set_model, prompt, compact.
- Validation is green: caco-web-observe 12 tests; caco-web lib 645 tests.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-web/static/app.js` — robust active-stream detection for steer routing.
  - `crates/caco-web/src/bin/caco-web-observe.rs` — streaming snapshot, steer submit step, delayed exit, expanded outbound assertion.
  - `crates/caco-web/src/tests.rs` — source guard for active-stream helper and steer routing.
  - `.cacophony/agent/.../summary/pending/web/steer-streaming-test/` — scenario evidence.
- Behavioural delta: plain Pico composer input now routes to shared `steer` whenever the shared snapshot still exposes active streaming content, improving native parity with Pico clients.

## Embedded artefacts

- `web/steer-streaming-test/pico-steer-observe.log` — scenario log with received steer/model/prompt/compact/ui_reply frames.
- `web/steer-streaming-test/pico-steer-server.log` — dev server log.
- `web/steer-streaming-test/screenshots/*.png` — scenario screenshots.
- `web/validation.txt` — command/results summary.

## Operator-takeaway

caco-web Pico now proves the full bidirectional composer path: while an answer is streaming, plain text becomes a `steer` HostRequest; after the stream settles, plain text becomes a normal prompt, and other controls still send model, compact, and dialog reply frames.
