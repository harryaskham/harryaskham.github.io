# bd-9bbaf2: collapse TUI mark-upload surface lookups

## What changed

- `SurfaceManager::mark_uploaded_with_image_id()` now captures `placement_id`, resolved displayed image ID, and previous displayed image ID from one immutable surface lookup.
- The active-placement release/increment logic is unchanged, but no longer re-probes the surface map just to read `displayed_image_id`.
- Added source-shape coverage and reran the focused `mark_uploaded` tests.

## Why

Every successful Kitty upload/retained redisplay funnels through `mark_uploaded_with_image_id()`. The old path performed two immutable HashMap lookups before the mutable update. Collapsing those reads trims per-upload overhead while preserving displayed-image accounting and stale-placement release behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_9bbaf2"` — `tj-5a470a07`, passed
- `caco test run --wait --command "cargo test -p caco-tui mark_uploaded"` — first enqueue hit transient daemon reachability, retry passed as `tj-4459af14`
