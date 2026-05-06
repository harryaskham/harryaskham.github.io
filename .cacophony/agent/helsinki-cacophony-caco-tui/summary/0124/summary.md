# bd-52c7b2: skip active background set replacement when unchanged empty

## What changed

- `App::flush_graphics_requests()` now skips `replace_graphics_surface_set()` for background surfaces when both the newly active background set and the previously tracked background set are empty.
- Replacement still runs whenever either side has entries, preserving stale-background retirement and active set updates.
- Added source-shape coverage for the empty-set guard.

## Why

Some graphics-capable frames have no background surfaces. The old path still entered the set replacement helper with two empty sets, paying equality/difference bookkeeping that cannot retire or activate anything. The guard trims that empty-frame overhead while keeping all cleanup paths intact once backgrounds exist.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_52c7b2"` — `tj-45d284e4`, passed
