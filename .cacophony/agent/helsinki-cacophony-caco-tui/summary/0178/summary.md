# bd-87f0f6: skip shared retained lookup when cache is empty

## What changed

- `SurfaceManager::retained_image_id()` now returns surface-local retained hits immediately.
- On local miss, it returns `None` before constructing/probing a shared retained key when `shared_retained_images` is empty.
- Non-empty shared retained lookup remains unchanged, preserving cross-surface retained redisplay of identical PNG payloads.
- Added focused source/runtime coverage for empty shared-cache misses, local hits, and non-empty shared fallback behavior.

## Why

Retained lookup runs for every cached Kitty upload candidate. On cold starts or local-only retained-cache states, local misses previously still constructed a shared key and probed an empty `HashMap`. Avoiding guaranteed-empty shared probes trims another small retained/redisplay hot path while keeping the shared payload cache behavior that avoids re-uploading duplicate graphics.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_87f0f6"` — passed (`tj-bdbd00ca`, rerun below)
- `caco test run --wait --command "cargo test -p caco-tui retained_image_lookup"` — passed (`tj-eae14e80`, rerun below)
