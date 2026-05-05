# bd-30ff81: skip stale graphics surface diff when active set is unchanged

## What changed

- Added `replace_graphics_surface_set()` for graphics surface lifecycle scopes.
- The helper fast-paths when the previous and active surface sets are equal, avoiding stale-key Vec allocation and retire loops that would do no work.
- Reused it for backgrounds, span glows, span pills, flat header decorations, cursor glows, and sparkline placements.
- Added regression coverage proving unchanged sets do not queue deletes or remove the live surface.

## Why

`flush_graphics_requests()` repeatedly built `previous.difference(active).cloned().collect()` vectors for multiple graphics scopes every frame. On steady-state frames those sets are usually unchanged and the vectors are empty. Fast-returning on equal sets trims allocation/scans while preserving de-draw correctness when a surface actually disappears.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_30ff81"` — `tj-629ea945`, passed
