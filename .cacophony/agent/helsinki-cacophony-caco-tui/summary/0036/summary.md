# bd-a8216b: skip singleton TUI graphics request preparation overhead

## What changed

- Added `prepare_graphics_requests()` as the shared post-collection preparation path for graphics request streams.
- The helper fast-returns for empty/singleton streams before building duplicate-key maps or running disambiguation/coalescing.
- Multi-request streams still perform same-key/different-rect suffixing and same-identity last-wins coalescing.
- Added regression coverage proving singleton streams skip the setter/disambiguation path.

## Why

Sparse graphics scenes often have zero or one span/cursor/header/sparkline request. Duplicates are impossible in those cases, but the flush path still paid duplicate-key map setup and coalescing scaffolding. This trims small per-frame overhead while preserving output and correctness for multi-request streams.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_a8216b"`
  - `tj-99cffb0f` matched zero tests before bead-specific coverage was added.
  - `tj-baeb16cf` and `tj-311ef740` caught Fn/FnMut compile issues in the test/helper boundary.
  - `tj-0b453fe4` passed with one warning.
  - `tj-8651028f` passed after removing the warning.
