# bd-342d88: reuse image-cache presence in TUI Kitty pending-work summary

## What changed

- `SurfaceManager::pending_graphics_work_summary()` now computes `has_cached_image` once per surface.
- The fetch classification reuses that value instead of calling `image_cache.contains_key(key)` directly.
- The regular-upload classification also reuses that value.
- Added a regression test ensuring the summary body has only one image-cache presence probe per surface and that both branches use the shared value.

## Why

The app-level graphics upload-pass gate calls `pending_graphics_work_summary()` on steady redraws. A surface with an image id and potential regular upload state could previously hash/probe `image_cache` twice while being classified. Reusing the cache-presence result trims unnecessary `HashMap` probes from the gate without changing fetch/upload/native/backoff classification.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_342d88"` — `tj-53c24447`, passed
