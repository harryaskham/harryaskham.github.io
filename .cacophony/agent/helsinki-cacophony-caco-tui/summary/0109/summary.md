# bd-94f1d6: avoid cloning TUI split background styles

## What changed

- `render_composite_cached_split_with_stats_ref()` now represents split background groups as contiguous `BackgroundLayerGroup` ranges instead of `Vec<usize>` lists.
- Split rendering passes borrowed `styles[group.start..group.end]` slices into `render_composite_cached_with_stats_ref()` instead of allocating/cloning `Vec<ResolvedGraphicsStyle>` for every group on each render.
- Tint slices are borrowed when the caller provides enough tints; a small fallback vector is only built for short/missing tint arrays.
- Added regression coverage that the split helper uses range groups/style slices and no longer clones `ResolvedGraphicsStyle` values in the split hot path.

## Why

Split background layers can appear on graphics-enabled panel fills. The old path grouped layer indices, then cloned each `ResolvedGraphicsStyle` into a per-group vector every frame before rendering. Since split groups are naturally contiguous, range groups let the renderer reuse existing slices and avoid unnecessary allocations/clones while preserving output and draw-above behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/background_renderer.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_94f1d6"` — `tj-996fc27d`, passed
