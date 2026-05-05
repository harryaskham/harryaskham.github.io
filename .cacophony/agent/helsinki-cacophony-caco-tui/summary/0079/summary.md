# bd-d7093a: keep shared retained Kitty lookup backed when image IDs are forgotten

## What changed

- `SurfaceManager::forget_retained_image_id()` now routes shared hash lookup cleanup through `remove_shared_retained_image_if_unbacked()`.
- Added regression coverage ensuring `forget_retained_image_id()` no longer directly removes `shared_retained_images[hash]` and uses the shared backed-lookup guard.

## Why

bd-2d6667 made retained removal/eviction keep a shared retained hash lookup alive until no retained variant for that hash/image remains. `forget_retained_image_id()` still had its own direct removal path, which could diverge from that invariant. Routing it through the same helper keeps cross-surface retained redisplay lookup consistent and avoids avoidable full reuploads after one alias path is forgotten.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_d7093a"` — `tj-d80caead`, passed
