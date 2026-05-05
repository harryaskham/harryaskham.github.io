# bd-05b774: coalesce same TUI graphics placement requests before flush

## What changed

- Replaced exact-only request dedupe with `coalesce_graphics_requests_by_identity()`.
- For each post-disambiguation graphics stream, requests are keyed by surface identity (`key/panel_id + rect`) and the last request for the same identity wins while preserving that identity's first position in the frame order.
- The helper remains hash-bucketed and uses caller-provided identity checks inside each bucket, so hash collisions and intentionally distinct identities are preserved.
- Added regression coverage proving same-placement requests retain first ordering but use the last emitted payload.

## Why

For a given graphics placement, repeated registrations in a single frame ultimately behave as last registration wins. Rendering earlier payloads first wastes text-decoration/border/background cache lookups and registration work, and can skew benchmark counters. Coalescing by placement identity trims that work while preserving the final drawn output and same-key/different-rect disambiguation.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_05b774"` — `tj-ee5b4e5e`, passed
