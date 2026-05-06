# bd-647c7d: skip image-cache probes when graphics image cache is empty

## What changed

- `SurfaceManager::pending_graphics_work_summary()` now hoists `image_cache_empty = self.image_cache.is_empty()` once per summary pass.
- Per-surface cached-image classification short-circuits to `false` while the cache is empty, avoiding a `HashMap::contains_key()` probe for every surface on cold/no-image frames.
- Fetch classification still reports daemon-backed surfaces needing image data when the cache is empty.
- Regular uploads are not reported without cached payloads.
- Added runtime/source coverage for the empty-cache fast path and kept the existing pending-summary source tests green.

## Why

Graphics-capable redraws call the pending-work summary to decide whether a Kitty upload pass is needed. On frames with registered surfaces but no cached image payloads, probing `image_cache` for every fetch/upload classification is guaranteed to miss. This makes the no-payload graphics hot path a little closer to ASCII/text overhead without changing observable rendering behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_647c7d"` — `tj-b2378d2a`, passed
- `caco test run --wait --command "cargo test -p caco-tui pending_graphics_summary"` — `tj-9e24efd1`, passed
