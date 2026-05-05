# bd-0ae0dc: borrow background cache entries without helper Vec allocation

## What changed

- Removed the intermediate `CachedBackgroundSurface` helper/vector from the app background cache-hit path.
- Cached hits now borrow the `graphics_background_cache` entry directly, record telemetry from the borrowed entry length, and iterate borrowed key/flag pairs in place.
- Keys are cloned only when inserted into the owned active-background set.
- Added regression coverage proving the direct-borrow path shape and absence of the helper allocation.

## Why

The previous slice removed repeated root lookups but did so by collecting owned key/flag pairs into a temporary vector before mutating surfaces. The final direct-borrow path avoids both repeated map probes and that intermediate allocation while still allowing safe mutation of disjoint fields (`surfaces`, `graphics_perf`, and active sets).

## Validation

- First targeted run failed because the regression assertion self-matched the removed helper type name.
- Fixed the assertion with `concat!`.
- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_0ae0dc"` — `tj-96c07b1a`, passed
