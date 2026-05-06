# bd-70437f: skip undecorated title fast-path lookup

## What changed

- Tightened the border title-decoration reuse helper used by unchanged/phase-cache fast paths.
- If the current panel/style does not require a title decoration, the helper now returns before probing `SurfaceManager` for the title-decoration key.
- Decorated panels still go through `ensure_title_decoration_surface(..., reuse_existing=true)` and therefore mark live or re-render as needed.
- Added regression/source coverage ensuring undecorated fast paths do not reintroduce a title-surface lookup.

## Why

Most panels have no title gap/header decoration. Even after avoiding unconditional retire misses, the fast path still performed `surfaces.get(surface_key)` for undecorated panels. That lookup is almost always a miss and happens per undecorated panel per stable graphics frame. Requirement changes already make the panel snapshot differ and use the full changed path; unexpected orphan decorations are also corrected by the redraw-wide stale-surface sweep if they are not marked live.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/border_integration.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_70437f"` — `tj-8e856604`, passed
