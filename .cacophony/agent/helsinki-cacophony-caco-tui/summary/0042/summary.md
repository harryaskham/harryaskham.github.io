# bd-d22758: skip TUI graphics coalescing when request keys are unique

## What changed

- `prepare_graphics_requests()` now performs a repeated-key preflight for streams larger than two requests.
- If all keys are unique, it returns before building duplicate-rect maps and before hash-bucket coalescing.
- Repeated-key streams still run the existing same-key/different-rect suffixing and same-identity last-wins coalescing path.
- Added regression coverage proving a three-entry all-unique stream keeps original key allocations and never calls the suffix setter.

## Why

Many graphics request streams have unique stable keys. In those cases there can be no same-key/same-rect coalescing and no same-key/different-rect suffixing. Avoiding generic preparation work trims steady-state overhead while preserving correctness for repeated-key cases.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_d22758"` — first attempt hit transient daemon reachability; retry `tj-eb01d63b`, passed
