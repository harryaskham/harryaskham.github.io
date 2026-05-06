# bd-1eeaf6: avoid active background root set on empty cache

## What changed

- `App::flush_graphics_requests()` now allocates `active_background_roots` only when `graphics_background_cache` was non-empty at frame start.
- Root insertion and `prune_inactive_background_cache_roots()` are conditional on that optional root set.
- Empty-cache frames skip root-set allocation/pruning because there are no inactive roots to remove; newly inserted roots on that frame are all active by construction.
- Added source-shape coverage for optional root tracking.

## Why

The background root set exists only to prune stale app-level background cache entries. When the cache starts empty, no stale roots can exist, but the old path still allocated and filled a `HashSet` for every background request. Skipping that work trims initial/empty-cache graphics frames while preserving pruning once prior cache roots exist.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- Initial `caco test run --wait --command "cargo test -p caco-tui bd_1eeaf6"` — `tj-2c7bd73b`, failed due rustfmt line wrapping in the source assertion.
- Adjusted assertion to match formatted source.
- Rerun `caco test run --wait --command "cargo test -p caco-tui bd_1eeaf6"` — `tj-6cac89ae`, passed
