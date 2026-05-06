# bd-d99f5f: skip fetch image-cache probes when cache is empty

## What changed

- `SurfaceManager::pending_fetches()` now hoists `image_cache_empty = self.image_cache.is_empty()` before walking surfaces.
- `pending_fetches()` skips per-surface `image_cache.contains_key(key)` probes entirely while the image cache is empty.
- `SurfaceManager::has_pending_fetch_candidates()` uses the same empty-cache short-circuit for its preflight path.
- Added focused source/runtime coverage showing the empty-cache fast path still reports daemon-backed fetch candidates and collects the expected fetch tuple.

## Why

Daemon-backed graphics image surfaces need fetch classification even before any payload has been cached. On cold/no-payload graphics frames, probing `image_cache` for each surface is guaranteed to miss. Hoisting the empty-cache state avoids those HashMap probes while preserving `fetch_pending`, `permanently_failed`, cached-surface exclusion, and fetch collection behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_d99f5f"` — `tj-615f49f1`, passed
- `caco test run --wait --command "cargo test -p caco-tui pending_fetches"` — `tj-04c44f1e`, passed
