# bd-20a7d8: prune non-renderable background cache without resolving layers

## What changed

- `App::flush_graphics_requests()` now skips `resolved_background_layers()` for panels whose raw background preflight says no background can render, even when app background cache state exists.
- Renderable panels still resolve layers, mark their active background root, and use normal cache/render paths.
- Stale cached roots for non-renderable panels are left unmarked so `prune_inactive_background_cache_roots()` removes them at the end of the flush pass.
- Added source-shape coverage to ensure the resolved-layer allocation sits behind the renderable-background guard and prune remains active.

## Why

When the app background cache is non-empty, the previous path still resolved layers for no-background panels just to discover/remove stale roots. Active-root pruning already handles stale roots; non-renderable panels do not need layer resolution, effective request cloning, snapshot construction, or cache lookup. This keeps cleanup correct with less per-panel work.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_20a7d8"` — `tj-48ff54e6`, passed
