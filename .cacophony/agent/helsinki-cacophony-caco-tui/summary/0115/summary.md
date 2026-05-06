# bd-932403: avoid duplicate surface lookups on image reserve

## What changed

- `SurfaceManager::reserve_with_image()` now captures existing surface state once before reserve/resize handling.
- Same-image rect movement, native-animation stop planning, and displayed-image release planning reuse that captured state instead of probing `surfaces` again.
- Displayed-image release is still skipped when `reserve()` already performed resize cleanup, preventing double-release/delete behavior.
- Added source-shape coverage plus ran the existing `reserve_with_image*` lifecycle regression suite.

## Why

Daemon-backed image surfaces can move or change while graphics are active. The previous path looked up the same surface repeatedly to classify same-image moves, native-animation replacement, and displayed-image cleanup before the mutable update. Reusing a pre-reserve snapshot trims surface-map probes while preserving flicker-free cached rect moves and stale-placement cleanup on uncached/image-change paths.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_932403"` — `tj-23a8a949`, passed
- `caco test run --wait --command "cargo test -p caco-tui reserve_with_image"` — `tj-d7a81bbb`, passed
