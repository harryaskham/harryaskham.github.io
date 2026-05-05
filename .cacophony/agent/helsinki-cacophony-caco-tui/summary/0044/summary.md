# bd-693493: skip TUI graphics disambiguation pass when duplicate keys share one rect

## What changed

- `prepare_graphics_requests()` now checks whether `duplicate_graphics_keys()` found any same-key/different-rect collisions before running the disambiguation pass.
- Repeated same-key/same-rect request streams skip per-rect suffixing and go straight to last-wins coalescing.
- Added regression coverage proving the setter/disambiguation callback is not called for same-key/same-rect repeats and the last payload wins.

## Why

Repeated same-key/same-rect graphics requests do not need per-rect suffixing. They only need identity coalescing. Avoiding the extra disambiguation loop trims one request-vector scan in that case while preserving output.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_693493"` — first attempt hit transient daemon reachability; retry `tj-61969504`, passed
