# bd-cd4c66: avoid hashing absent background preflight instances

## What changed

- Changed `BackgroundPreflightKey` from a struct with presence/length/hash fields to an enum:
  - `Role(PanelRole)` for requests without an instance.
  - `Instance { role, instance_len, instance_hash }` for instance-bearing requests.
- `BackgroundPreflightKey::for_request()` now returns the role-only variant before doing any instance length/hash work.
- Instance-bearing requests still use the compact borrowed-data fingerprint path introduced earlier.
- Added source-shape coverage for the role-only fast path and instance fingerprint variant.

## Why

Most graphics border/background requests do not carry an instance. The previous key shape encoded absence with zero length/hash fields, which was cheap but less explicit and could make future changes accidentally do unnecessary instance work. The enum makes the no-instance path a distinct fast path, keeping common role-only preflight cache keys minimal while preserving repeated-instance cache behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_cd4c66"` — `tj-7066a929`, passed
