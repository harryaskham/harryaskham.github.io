# bd-a23be5: cache TUI border title decoration keys

## What changed

- `PanelSurfaceKeys` now caches the title/header decoration surface key alongside the eight border segment keys.
- Border title-decoration registration receives the cached key by reference instead of formatting `enh:title_decor:<panel_id>` inside the hot path.
- Cached-surface liveness checks, stale-panel retirement, and modal suppression reuse the cached title key when panel key state is available, falling back to formatting only for legacy/missing-key recovery paths.
- Added regression coverage that the cached title key exists and is used by registration/liveness paths.

## Why

The border integration already precomputes segment keys to avoid repeated string formatting on steady graphics frames, but title/header decoration paths still rebuilt their key during mark-live, registration, modal suppression, and stale-panel cleanup. Caching the title key with the rest of the per-panel surface keys keeps border de-draw correctness while shaving avoidable allocation/formatting from graphics hot paths.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/border_integration.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_a23be5"` — `tj-bf01bdb2`, passed
