# bd-736f43: avoid duplicate background cache map lookups on hits

## What changed

- Added `CachedBackgroundSurface` and `cached_background_surfaces_for_root()` to collect owned cached key/flag pairs from a single app background-cache root lookup.
- The cached hit path now uses that helper instead of looking up `background_root_key` once for length and again for every cached surface index.
- Existing test helper behavior remains compatible by converting the helper's owned key/flag pairs back into vectors.
- Added regression coverage for the single-helper path shape.

## Why

The previous cache-hit path avoided borrow conflicts by re-looking up the same root entry per cached surface. That kept correctness but added repeated HashMap probes on steady cached background frames. The helper collects the small owned key/flag pairs once, releases the cache-map borrow, and then lets the caller update surfaces/perf/active sets safely.

## Validation

- First targeted run failed because the regression assertion self-matched its forbidden string.
- Fixed the assertion with `concat!`.
- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_736f43"` — `tj-b2020076`, passed
