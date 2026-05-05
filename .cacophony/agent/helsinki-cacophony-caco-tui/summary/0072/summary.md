# bd-725889: use one classification pass for TUI Kitty pending fetches

## What changed

- `SurfaceManager::pending_fetches()` now uses `filter_map()`.
- Each surface's `image_id` presence, fetch eligibility, cache presence, and returned `(surface_key, image_id)` clone happen in one closure.
- Removed the previous filter-then-map shape that cloned with `image_id.clone().unwrap()` after eligibility filtering.
- Added regression coverage for the single-pass/filter-map shape.

## Why

`pending_fetches()` is part of the graphics upload-pass preflight/fetch path for daemon-backed images. Combining classification and cloning avoids a second iterator closure and an unnecessary `unwrap`, keeping behavior unchanged while trimming small per-surface overhead.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_725889"` — `tj-d30fa206`, passed
