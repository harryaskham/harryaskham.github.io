# bd-68231a: avoid allocations in two-entry TUI graphics prep fast path

## What changed

- Reworked the two-entry branch in `prepare_graphics_requests()` to borrow keys for the common distinct-key comparison.
- It now returns for distinct keys before allocating/cloning any key strings.
- It still coalesces same key+rect with last-wins payload semantics and only allocates a shared key string when same-key/different-rect suffixing is needed.
- Added regression coverage that verifies distinct two-entry prep preserves the original key allocations/pointers.

## Why

bd-d3117b made two-entry streams avoid the generic duplicate-map/hash-bucket path, but it still cloned both keys before learning whether they were distinct. Distinct two-entry graphics streams are common in sparse scenes and should be effectively free.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_68231a"` — `tj-c1991f60`, passed
