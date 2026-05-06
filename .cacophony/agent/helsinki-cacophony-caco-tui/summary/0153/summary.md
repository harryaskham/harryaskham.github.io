# bd-9080e1: avoid hashing background preflight instances repeatedly

## What changed

- Added `BackgroundPreflightFirstEntry`, which stores the first role/instance preflight result and compares repeated requests against borrowed request data.
- The first-entry cache hit is checked before constructing a `BackgroundPreflightKey`, so repeated same-instance requests do not hash/fingerprint the instance again.
- The promoted multi-entry cache still uses compact `BackgroundPreflightKey` fingerprints.
- Added source-shape coverage ensuring first-entry hits precede fingerprint-key construction.

## Why

The compact preflight key avoided storing/cloning instance strings in the multi-entry cache, but repeated same-instance first-entry hits still built a fingerprint each time before checking the first cache entry. Checking the first entry directly avoids repeated instance hashing in the common single-role/instance frame while preserving multi-entry behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_9080e1"` — `tj-f2f8c2b5`, passed
