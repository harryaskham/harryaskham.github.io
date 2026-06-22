# Session summary — bd-546608: outbound Pico /think HostRequest proof

## Goal

Extend caco-web Pico bidirectional command coverage to reasoning-effort controls by proving `/think <level>` routes through the shared composer and leaves the browser as the canonical HostRequest.

## Bead(s)

- `bd-546608` — [pico] caco-web: mock websocket scenario must capture outbound /think HostRequest.

## Before state

- The mock scenario proved prompt, steer, abort, `/models`, compact, model picker, and dialog reply traffic.
- It did not submit a reasoning-effort command.

## After state

- The `pico-pane` scenario submits `/think high` through the real Pico composer.
- The mock backend asserts:
  - `{"kind":"command","type":"set_thinking_level","level":"high"}`
- Existing outbound assertions remain intact: confirm ui_reply, steer, abort, input ui_reply, get_available_models, set_model, prompt, compact.
- Select-dialog and dropped-websocket reconnect subscenarios still pass.
- Validation is green: caco-web-observe 12 tests; caco-web lib 646 tests.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-web/src/bin/caco-web-observe.rs` — `/think high` submit step, outbound assertion, deterministic timing adjustments.
  - `.cacophony/agent/.../summary/pending/web/think-command-test/` — scenario evidence.
- Behavioural delta: no production runtime change; live browser proof now covers reasoning-level command traffic.

## Embedded artefacts

- `web/think-command-test/pico-think-observe.log` — scenario log with received `set_thinking_level` frame.
- `web/think-command-test/pico-think-server.log` — dev server log.
- `web/think-command-test/screenshots/*.png` and page snapshots — scenario artifacts.
- `web/validation.txt` — command/results summary.

## Operator-takeaway

caco-web Pico now proves reasoning-effort commands over the structured WebSocket path, alongside prompt/steer/abort/model/dialog/compact coverage.
