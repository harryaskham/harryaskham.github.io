# bd-147f7f: avoid single-layer TUI background tint Vec

## What changed

- `App::render_graphics_background_with_layers()` now uses a stack one-element tint array for the common single-layer background path.
- Multi-layer/split backgrounds still allocate and pass a tint `Vec` slice as before.
- Added regression coverage that the render helper uses `if let [layer] = layers`, keeps a stack tint path, and no longer unconditionally allocates `Vec<[u8; 4]>` for every background request.

## Why

Most graphics panel fills are single-layer. The old implementation allocated a tint vector for every background request even when only one layer existed. Using a stack slice removes avoidable allocation from steady graphics frames while preserving all composite/split/native rendering behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_147f7f"` — `tj-bd404b8f`, passed
