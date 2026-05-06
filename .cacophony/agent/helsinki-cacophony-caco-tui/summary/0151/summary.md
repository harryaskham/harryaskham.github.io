# bd-f1ea92: avoid background preflight first-entry clone on repeated hit

## What changed

- Added `BackgroundPreflightKey`, a compact copyable key for the background preflight cache.
- The key stores the panel role plus instance presence/length/FNV hash derived from borrowed `request.instance.as_deref()`.
- `flush_graphics_requests()` now builds background preflight cache keys without cloning `request.instance`.
- The one-entry/no-Vec cache and promoted small-Vec cache both use the compact key.
- Background image override requests continue to bypass the cache and force background handling.
- Updated source-shape coverage for no-clone key construction and existing small-cache reuse semantics.

## Why

The previous lazy preflight cache avoided allocating a Vec for single-use frames, but it still cloned `request.instance` when materializing the first cache entry. Repeated role/instance hits could then reuse that entry, but the first cacheable request still paid a String clone. Fingerprinting borrowed instance text avoids that allocation while preserving cheap repeated-hit cache comparisons.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_f1ea92"` — `tj-a95777fd`, passed
