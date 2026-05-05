# bd-d11e99: deduplicate exact non-border TUI graphics requests before flush

## What changed

- Extended exact duplicate request filtering beyond border requests to:
  - span glows
  - span pills
  - flat header decorations
  - cursor glows
  - sparkline placements
- Added `PartialEq` derives for the non-border request structs and `PartialEq/Eq` for `SparklineSegment` so the shared `retain_unique_graphics_requests()` helper can preserve first occurrence order while dropping exact repeats.
- Kept same-key/different-rect suffixing in place before dedupe, so distinct placements remain distinct and rendered output is unchanged.

## Why

Duplicate view emissions for non-border graphics could trigger redundant text-decoration/sparkline cache lookups, registrations, live-surface work, and benchmark noise without changing the final frame. This trims avoidable steady-state graphics flush work in the same spirit as bd-f5f605 for border requests.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs crates/caco-tui/src/views/common.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_d11e99"` — `tj-594e73b6`, passed
