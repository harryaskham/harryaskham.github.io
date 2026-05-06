# bd-4425b1: skip image-cache lookup for non-regular upload candidates

## What changed

- `SurfaceManager::pending_uploads()` now rejects native-animation surfaces before probing `image_cache`.
- Non-animation redraws of already-uploaded animated surfaces now increment the skipped-animation diagnostic and return before probing `image_cache`.
- Added regression coverage that those gates precede the first `image_cache.get(key)?` probe in `pending_uploads()`.

## Why

`pending_graphics_work_summary()` already narrows regular upload candidates to non-uploaded surfaces, but the full `pending_uploads()` pass can still be reached alongside other graphics work. Rejecting non-regular candidates before `image_cache` access avoids unnecessary HashMap probes on mixed graphics frames while preserving backoff ticking, native-animation handling, and skipped-animation accounting.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_4425b1"` — `tj-af9b38aa`, passed
