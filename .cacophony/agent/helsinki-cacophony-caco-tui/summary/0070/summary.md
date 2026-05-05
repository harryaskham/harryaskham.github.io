# bd-f6fc99: avoid post-budget map lookups for TUI Kitty native animation clones

## What changed

- `SurfaceManager::pending_native_animation_uploads()` now carries a `&NativeAnimation` reference through candidate collection, sorting, and upload-budget truncation.
- Selected post-budget entries clone the carried reference directly.
- Removed the post-budget `self.surfaces.get(&key)` lookup previously used to recover the animation payload for selected entries.
- Kept bd-69cd9b behavior: native animation payloads are still cloned only after sorting and `max_uploads_per_frame` truncation.
- Added regression coverage proving selected entries clone without post-budget surface-map lookup.

## Why

bd-69cd9b removed pre-budget animation cloning, but selected entries still had to look themselves back up in `self.surfaces` by key before cloning. Carrying references through the lightweight sort/truncation path keeps delayed cloning while avoiding per-selected-entry `HashMap` probes.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_f6fc99"` — `tj-211c3639`, passed
