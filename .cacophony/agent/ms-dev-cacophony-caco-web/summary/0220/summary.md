# Session summary — bd-7af724: shared Pico composer parser recognizes /abort

## Goal

Move the caco-web `/abort` fix into the shared `caco-picophony` composer parser so every Pico surface treats `/abort` as a built-in abort command, and caco-web no longer needs a local exact-string special-case.

## Bead(s)

- `bd-7af724` — [pico] shared composer parser must recognize /abort for caco-web.

## Before state

- caco-web had to special-case exact `/abort` with `adapter.abortLine()`.
- Shared `parse_composer_line("/abort")` fell through to `Prompt`, which caused caco-web to send `{"type":"steer","message":"/abort"}` while streaming before bd-a9b295.

## After state

- `/abort` is in the shared `BUILTIN_COMMANDS` registry and `/help` output.
- `parse_composer_line("/abort")` returns `ComposerAction::Command(RpcCommand::Abort)`.
- wasm `PicoView::parse_composer("/abort")` returns a command whose type is `abort`.
- caco-web removed the local `/abort` special-case and now relies on shared `parseComposer` + `commandLine` like other built-ins.
- Browser wasm artifact `pico_view_bg.wasm` was regenerated.
- Live `caco-web-observe --scenario pico-pane` still receives the canonical abort frame, plus steer, ui_reply, get_available_models, set_model, prompt, and compact.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-picophony/src/commands.rs` — shared parser, built-in registry/help, tests.
  - `crates/caco-picophony/src/wasm.rs` — wasm parse-composer test.
  - `crates/caco-web/static/app.js` — removed local `/abort` branch.
  - `crates/caco-web/static/pico_view_bg.wasm` — regenerated browser artifact.
  - `crates/caco-web/src/tests.rs` — source guard updated for shared-path routing.
  - `.cacophony/agent/.../summary/pending/web/shared-abort-test/` — scenario evidence.
- Behavioural delta: `/abort` is now canonical shared Pico composer behavior, not a caco-web-only patch.

## Embedded artefacts

- `web/shared-abort-test/pico-shared-abort-observe.log` — scenario log with received abort frame after local special-case removal.
- `web/shared-abort-test/pico-shared-abort-server.log` — dev server log.
- `web/shared-abort-test/screenshots/*.png` — scenario screenshots.
- `web/validation.txt` — command/results summary.

## Operator-takeaway

The caco-web abort fix is now properly upstreamed into shared Picophony: browser, wasm, and other Pico surfaces parse `/abort` consistently as a built-in abort command.
