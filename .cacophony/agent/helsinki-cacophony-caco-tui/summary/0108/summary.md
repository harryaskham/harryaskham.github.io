# bd-6eb7b0: remove redundant TUI background cache rechecks

## What changed

- Removed an impossible second `shared_cache.get(&key)` probe in `BackgroundRenderer::render_cached_with_stats_ref()` after the fast-path hit check has already returned.
- Removed the equivalent redundant `composite_shared_cache.get(&composite_key)` recheck in `render_composite_cached_with_stats_ref()`.
- Simplified composite disk-cache store logic now that the disk-cache branch already has the cache handle.
- Added regression coverage that hit fast paths remain present and miss fallback no longer rechecks the shared-cache maps.

## Why

The background renderer already exits early on active shared-cache hits and inactive-cache promotions. The later fallback `let data = if let Some(...)` repeated the same HashMap lookup on every miss before going to disk/render. Removing the duplicate probe trims steady graphics-frame overhead in background miss paths without changing cache-hit, inactive-promotion, disk-cache, or render behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/background_renderer.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_6eb7b0"` — `tj-0ecc2ba3`, passed
