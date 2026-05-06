# bd-2bf356: avoid image-path Vec for non-image composite backgrounds

## What changed

- `BackgroundRenderer::render_composite_cached_with_stats_ref()` now allocates `resolved_image_paths` only when the composite includes an image layer.
- Non-image composite backgrounds borrow the original `styles` slice directly for rendering instead of building a cloned `render_styles` vector.
- Image-layer composites still build resolved render styles so avatar/background rotation continues to use the selected image path.
- Added regression coverage that image-path storage is gated by `has_image_layer` and that non-image render styles fall back to the borrowed style slice.

## Why

Many multi-layer backgrounds are pure color/glow/gradient composites. The old path still allocated an image-path vector and a cloned render-style vector on every composite render, even when no layer could use an image path. Gating that work keeps image-layer behavior intact while trimming allocations/clones from common non-image composite frames.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/background_renderer.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_2bf356"` — `tj-b8f1673d`, passed
