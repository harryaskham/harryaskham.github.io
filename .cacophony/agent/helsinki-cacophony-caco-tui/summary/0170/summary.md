# bd-aa2fb1: skip inactive background LRU scans on new stash

## What changed

- `BackgroundRenderer::stash_inactive_background()` now removes an existing inactive-LRU entry only when the inactive cache insertion actually replaced a key.
- `BackgroundRenderer::stash_inactive_composite()` now applies the same replacement-gated LRU retain behavior for composite background stashes.
- New inactive background/composite stashes still append to their LRU queues and evict by byte budget as before.
- Replacement stashes still subtract the old byte count, move the key to the back of the LRU, and avoid duplicate LRU entries.
- Added focused source/runtime coverage for replacement-gated retain behavior, inactive byte accounting, and duplicate-free replacement semantics.

## Why

Scene switches and background retirement can stash several rendered graphics assets into inactive caches to preserve warm reuse. The previous path scanned the inactive `VecDeque` with `retain` on every stash even when inserting a brand-new key. Gating the scan behind actual replacement trims background cleanup/retirement overhead while preserving cache correctness.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/background_renderer.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_aa2fb1"` — `tj-14d06eaa`, passed
- `caco test run --wait --command "cargo test -p caco-tui inactive_background"` — `tj-682de06e`, passed

Earlier focused runs (`tj-3b146395`, `tj-1fd169a2`, `tj-36dc1bb9`, `tj-77b414fb`) failed because the initial source-shape assertion anchored on the wrong occurrence of `inactive_shared_cache`; the implementation was retained and the assertion was corrected before passing validation.
