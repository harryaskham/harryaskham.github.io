# bd-9692b6: reuse TUI background snapshot across cache miss rendering

## What changed

- Added `BackgroundCacheLookup` to carry both the prepared `BackgroundSurfaceSnapshot` and optional cached surface keys/flags.
- `background_cache_lookup_for_layers()` now prepares the snapshot once for cache comparison.
- On cache misses, `flush_graphics_requests()` reuses that prepared snapshot when inserting the rendered background cache entry.
- Removed an unused compatibility wrapper after validation exposed the warning.
- Extended the regression coverage to ensure cache-miss insertion uses the prepared snapshot path.

## Why

Even after resolved background layers were reused across cache lookup/render, the cache-miss path still rebuilt the same `BackgroundSurfaceSnapshot` after rendering. Snapshot construction includes image-identity collection for image layers and request/style data comparisons. Reusing the lookup snapshot removes duplicate work from cache-miss frames while preserving the same correctness checks.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_9692b6"`
  - `tj-0ef276e2` passed but emitted an unused-method warning.
  - first retry hit transient daemon reachability.
  - `tj-24028445` passed cleanly after removing the unused wrapper.
