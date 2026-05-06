# bd-b0a541: skip image-source LRU scans on new cache inserts

## What changed

- `BackgroundRenderer::cache_decoded_image()` now calls `decoded_image_lru.retain(...)` only when the `HashMap::insert` replaced an existing decoded image key.
- `BackgroundRenderer::cache_resized_image()` now does the same for `resized_image_lru.retain(...)`.
- New cache-miss inserts still push the key to the LRU and evict by capacity as before.
- Replacement inserts still move the existing key to the back of the LRU without duplicating entries.
- Added focused source/runtime coverage for the replacement-gated retain behavior and duplicate-free LRU updates.

## Why

Image-backed graphics backgrounds can populate decoded/resized source caches during scene changes and preflight. The miss path had just proven the key absent, but every new insert still scanned the LRU `VecDeque` with `retain`. Gating that scan behind actual replacement avoids unnecessary cache-maintenance work while preserving correctness for rare explicit updates.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/background_renderer.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_b0a541"` — `tj-9b42d01c`, passed
- `caco test run --wait --command "cargo test -p caco-tui image_source_cache"` — `tj-de813f1e`, passed

Note: an earlier focused run (`tj-4fb3eda0`) failed because the initial source-shape assertion did not account for rustfmt line wrapping. The runtime behavior was then covered with the updated assertion and passed.
