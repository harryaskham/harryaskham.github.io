# bd-2d6667: keep shared retained Kitty lookup backed until all aliases are gone

## What changed

- Added `retained_hash_image_still_tracked()` and `remove_shared_retained_image_if_unbacked()` helpers.
- Per-surface eviction, global byte-budget eviction, and `remove_retained()` now remove `shared_retained_images[hash]` only when no retained variant for that hash/image remains.
- Added regression coverage for removing one shared alias while another retained variant still backs the shared lookup.

## Why

Shared retained aliases let multiple logical surfaces reuse one terminal-retained Kitty payload. Removing or evicting one alias previously dropped the global hash lookup whenever it pointed at that image id, even if the owner or another alias still tracked the same hash/image. That would break later cross-surface retained redisplay lookup and force avoidable full bitmap uploads. The shared lookup now lives as long as any retained variant still backs it.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_2d6667"` — `tj-f225379f`, passed
