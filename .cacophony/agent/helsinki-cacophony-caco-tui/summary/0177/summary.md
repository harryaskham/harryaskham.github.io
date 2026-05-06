# bd-a768c4: skip retained global LRU scan on new entry

## What changed

- `SurfaceManager::mark_retained()` now tracks whether a retained key/hash updated an existing variant.
- `global_retained_lru.retain(...)` only runs for replacement updates, where it is needed to move the existing entry to the back without duplication.
- New retained variants skip the global LRU scan and append their new `(key, data_hash)` entry directly.
- Replacement semantics, global LRU ordering, retained byte accounting, eviction cleanup, and duplicate-free updates are preserved.
- Added focused source/runtime coverage for the replacement guard and global LRU ordering.

## Why

Retained image bookkeeping is on the cached-Kitty hot path. When a retained variant is newly inserted, the per-surface variants map has just proven the key/hash absent, so scanning `global_retained_lru` for that same entry is unnecessary. This trims retained-cache maintenance while keeping replacement move-to-back correctness.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_a768c4"` — passed (`tj-6e6264d6`, rerun before commit below)
- `caco test run --wait --command "cargo test -p caco-tui mark_retained"` — passed (`tj-295ea092`, rerun before commit below)
