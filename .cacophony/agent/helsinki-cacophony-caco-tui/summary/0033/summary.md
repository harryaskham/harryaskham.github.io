# bd-e90ae2: use hashed fingerprints for TUI graphics request dedupe

## What changed

- Replaced the order-preserving exact duplicate graphics-request filter's `Vec::contains` scan with hash-bucketed dedupe.
- Request streams now pass a cheap key+rect fingerprint into the helper; exact equality is only checked inside matching fingerprint buckets to preserve correctness under hash collisions and for same placement with different payloads.
- Added regression coverage showing that identical requests are removed but distinct payloads sharing the same key/rect fingerprint are retained.

## Why

bd-f5f605 and bd-d11e99 removed duplicate border and non-border graphics requests, but the generic helper used an O(n²) full-vector scan. That could add avoidable overhead for high-cardinality graphics streams such as sparklines. The new fingerprint buckets keep the optimization itself from becoming a steady-state performance cost.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_e90ae2"` — first attempt hit transient daemon reachability; retry `tj-b96e5a91` passed
