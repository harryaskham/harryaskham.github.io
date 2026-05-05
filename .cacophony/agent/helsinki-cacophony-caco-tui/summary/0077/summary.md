# bd-d46302: preserve retained-byte ownership when shared Kitty image id is replaced

## What changed

- Added regression coverage for replacing a local payload-owning retained variant with a globally shared retained image id for the same hash.
- The existing retained-byte accounting path correctly subtracts the old local payload bytes and records the replacement as a zero-byte shared alias.

## Why

Shared retained image aliases should not inflate global retained-byte accounting. The first-alias and repeated-alias cases were already covered, but the owner-to-shared replacement case is the scenario that can happen when a surface initially uploads a payload and later switches to a shared retained id. This regression ensures retained byte totals drop back to the true terminal payload ownership, reducing premature retained eviction and avoiding unnecessary full uploads.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_d46302"` — `tj-6ce73d94`, passed
