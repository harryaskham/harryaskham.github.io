# bd-5113b7: avoid background request clone when animation unchanged

## What changed

- Split the background animation decision into `effective_background_animation_enabled()`.
- `flush_graphics_requests()` now computes the effective background animation bit directly.
- The background path borrows the original `GraphicsBorderRequest` when `request.animate` is already correct for background rendering.
- It clones the request only when it must alter the animation bit for background-specific behavior.
- Updated related source-shape tests so the single layer scan and empty-background clone guard assertions match the new borrowed/clone-on-change path.

## Why

Background-capable panels previously cloned the whole `GraphicsBorderRequest` to build an effective background request even when the computed background animation flag matched the original. Many panels already have `animate=false`, so the clone was unnecessary. Borrowing the original request on unchanged animation trims per-panel allocation/copy overhead while preserving the static-background animation gating that prevents raster churn.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_5113b7"` — `tj-42dccd4f`, passed
