# bd-1398e4: skip local retained lookup when cache is empty

## What changed

- `SurfaceManager::retained_image_id()` now guards the surface-local retained lookup with `!self.retained_images.is_empty()`.
- Empty local retained caches skip the local `HashMap::get(key)` probe before reaching the existing shared-cache empty guard/fallback.
- Local retained hits and non-empty shared retained fallback behavior remain intact.
- Added focused source/runtime coverage for empty local misses and normal local hits.

## Why

Retained lookup runs for every cached Kitty upload candidate. On cold starts or before any retained image is tracked, the local retained cache is empty; probing it is guaranteed miss work. This complements the empty shared-cache guard and trims retained lookup overhead on cold/no-retained frames.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_1398e4"` — `tj-69ed0a8c`, passed
- `caco test run --wait --command "cargo test -p caco-tui retained_image_lookup"` — `tj-342014b1`, passed
