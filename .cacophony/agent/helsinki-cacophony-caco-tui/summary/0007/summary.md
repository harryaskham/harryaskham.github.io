# bd-be12e8: default procedural background quantization on

## What changed

- Changed `GraphicsConfig::default().scaled_background_cache` to `true`.
- Changed empty `GraphicsConfigOverride::to_full()` to default `scaled_background_cache` to `true`.
- Preserved explicit `scaledBackgroundCache: false` as the rollback / exact-raster diagnostic path.
- Updated docs/schema HTML and `SPEC.md` to match the new default.

## Why

The TUI already had a scaled procedural background cache, but it was opt-in. That meant default graphics mode still rasterized exact-size solid/gradient/glow/scanline backgrounds for near-identical panel dimensions. Enabling size quantization by default improves cache reuse for procedural backgrounds while preserving exact geometry for image backgrounds.

## Validation

- `rustfmt --edition 2021 --check crates/caco-config/src/model.rs`
- `git diff --check`
- `docs/validate-pages.sh`
- `caco test run --wait --command "cargo test -p caco-config graphics_config_defaults_enable_scaled_background_cache_bd_be12e8"` — `tj-75fe6f3e`, passed
