# bd-8b071d: avoid HashSet allocation for small TUI graphics request prep

## What changed

- Added `has_repeated_graphics_request_key()`.
- For graphics request streams with 8 or fewer entries, repeated-key detection now uses a small nested slice scan and avoids allocating a `HashSet`.
- Larger streams keep the `HashSet` path.
- The existing all-unique early return, duplicate-key suffixing, and same-identity last-wins coalescing behavior are preserved.
- Added regression coverage for small unique and repeated-key streams.

## Why

bd-d22758 avoided generic coalescing when all keys are unique, but its preflight still allocated a `HashSet` for streams larger than two. Many TUI graphics streams are small (3–8 entries), so a tiny stack/iterator scan is cheaper than heap allocation and hashing in the common all-unique case.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_8b071d"` — `tj-79fa9a80`, passed
