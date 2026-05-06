# bd-bd5cee: avoid background active-root clone on cache-hit frames

## What changed

- Moved background active-root marking out of the front of the background cache/render path.
- On app-level background cache hits, `flush_graphics_requests()` now marks the root active after the cache entry has been read, moving the owned `background_root_key` into `active_background_roots` without cloning.
- Render/cache-insert paths still clone the root only when both active-root tracking and cache insertion need ownership of the same key.
- Added source-shape coverage to keep the cache-hit path moving the key while preserving clone-on-insert behavior where it is still necessary.

## Why

When app background cache roots are being tracked, the previous path cloned every renderable background root before knowing which branch would use it. On cache-hit frames, the root key is only needed for lookup until the cached entry is processed; after that it can be moved into the active root set. This trims a `String` clone from the common cached-background path.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_bd5cee"` — `tj-838e6bf4`, passed
