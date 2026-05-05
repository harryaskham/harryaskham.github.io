# bd-024c67: avoid duplicate TUI background layer resolution on cache hits

## What changed

- Added `effective_background_request_for_layers()` so callers that already resolved background layers can derive the effective animation bit without resolving config layers again.
- `cached_background_surface_keys()` now resolves background layers once and reuses that same layer set for:
  - effective background animation handling,
  - `BackgroundSurfaceSnapshot` construction.
- Added regression coverage asserting the cache-hit fast path no longer uses the old `effective_background_request()` + second layer-resolution shape.

## Why

The app-level background cache-hit path is supposed to be the cheapest steady-state path: mark cached background surfaces live and skip renderer/raster/upload work. It still resolved graphics layers twice per panel: once to decide whether background animation was effectively enabled, and again for the snapshot. Reusing one resolved layer set trims avoidable config/style work on every cached graphics frame.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_024c67"`
  - first attempt hit transient daemon reachability
  - retry `tj-467228bd`, passed
