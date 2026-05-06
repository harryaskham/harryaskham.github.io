# bd-2467e5: lazily allocate active background surface set

## What changed

- `App::flush_graphics_requests()` now keeps `active_backgrounds` as an optional `HashSet`.
- The set is allocated at frame start only when previously tracked background surfaces may need stale cleanup.
- Current background cache-hit/render paths allocate the set on demand only once an actual background surface exists.
- Empty graphics-capable frames with no previous/current backgrounds skip active-background set allocation and skip replacement work.
- Added source-shape coverage for lazy active-background allocation.

## Why

The graphics path should not pay avoidable allocation/bookkeeping costs on frames that do not emit background graphics. The previous code allocated an empty `HashSet` unconditionally and later proved it was empty. This trims one more steady/empty-frame cost while preserving cleanup when previous or current background surfaces exist.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- Initial `caco test run --wait --command "cargo test -p caco-tui bd_2467e5"` — `tj-0f29bcbf`, failed due rustfmt line wrapping in the source assertion.
- Adjusted the assertion to match formatted source.
- Rerun `caco test run --wait --command "cargo test -p caco-tui bd_2467e5"` — `tj-e095fa5b`, passed
