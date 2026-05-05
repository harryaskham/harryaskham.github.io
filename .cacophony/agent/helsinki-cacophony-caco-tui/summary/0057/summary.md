# bd-02277e: reuse resolved TUI background layers across cache miss rendering

## What changed

- `flush_graphics_requests()` now resolves background layers once per border/background request and reuses that layer set for:
  - effective background animation calculation,
  - app-level background cache lookup,
  - cache-miss background rendering,
  - stored `BackgroundSurfaceSnapshot` construction.
- Added `cached_background_surface_keys_for_layers()` so cache-hit checks can consume caller-provided resolved layers.
- Added `render_graphics_background_with_layers()` so cache-miss rendering can consume caller-provided resolved layers.
- Kept existing `effective_background_request()`, `cached_background_surface_keys()`, and `render_graphics_background()` wrappers for existing tests/callers.
- Added regression coverage to prevent reintroducing the old duplicate-resolution call shape.

## Why

After bd-024c67, cache hits reused one layer set inside `cached_background_surface_keys()`, but the broader flush pipeline still resolved layers before cache lookup, inside cache lookup, inside cache-miss rendering, and again for snapshot insertion. On graphics-heavy frames, that duplicate config/style work happens per panel. Reusing a single resolved layer set keeps both cache-hit and cache-miss paths closer to the ASCII/text overhead once graphics surfaces are warm.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_02277e"`
  - `tj-809fcf84` failed because the assertion matched its own forbidden string.
  - `tj-566f5594` passed after splitting the assertion string.
