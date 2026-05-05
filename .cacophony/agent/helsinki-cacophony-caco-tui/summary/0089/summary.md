# bd-5425b6: skip background cache GC rebuild on steady frames

## What changed

- Added a steady-state fast path to `BackgroundRenderer::retain_surfaces()`.
- When all tracked surface mappings are active and all active shared background/composite cache entries are still referenced, the function returns before rebuilding active cache maps or allocating referenced-key `HashSet`s.
- Kept the existing stale cleanup path for changed surface mappings, image counters, single-layer cache entries, and composite cache entries.
- Added regression coverage for the stable surface/cache-map case.

## Why

`retain_surfaces()` runs on every graphics frame after background requests are processed. On steady dashboard frames, active background surfaces and referenced cache keys often do not change, but the old path still retained maps, collected referenced keys, rebuilt shared caches, and considered inactive stashing. The fast path avoids that repeated GC work while preserving correctness whenever a surface disappears or a cache entry becomes unreferenced.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/background_renderer.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_5425b6"` — `tj-6c531057`, passed
