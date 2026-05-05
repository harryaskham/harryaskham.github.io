# bd-d3117b: fast-path two-entry TUI graphics request preparation

## What changed

- `prepare_graphics_requests()` now has a two-request fast path:
  - distinct keys return immediately without duplicate-map/coalescing allocations,
  - same key + same rect coalesces to the last request payload,
  - same key + different rect directly applies per-rect suffixes to both requests.
- Larger request vectors keep the generic duplicate-key disambiguation and last-wins coalescing path.
- Added regression coverage for the two-entry same-identity and same-key/different-rect cases.

## Why

Sparse graphics scenes often emit two requests. The prior singleton fast path still sent two-entry streams through duplicate map construction and hash-bucket coalescing. The two-entry path handles the common cases directly, trimming per-frame overhead while preserving correctness and final output.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_d3117b"` — `tj-171b2bdb`, passed
