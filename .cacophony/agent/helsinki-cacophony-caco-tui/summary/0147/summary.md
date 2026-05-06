# bd-1d7a8e: avoid background preflight cache allocation on single-use frames

## What changed

- Reworked the per-frame background preflight cache in `App::flush_graphics_requests()` into a two-stage structure:
  - `background_preflight_first` stores the first role/instance preflight result with no `Vec` allocation.
  - `background_preflight_cache` is an optional small `Vec` allocated only when a second distinct preflight entry is needed.
- Repeated hits against the first entry still reuse the cached result.
- Multi-entry frames promote the first entry into the Vec and continue using the small-cache lookup path.
- Background image override requests still bypass the cache and force background handling.
- Updated source-shape coverage for the lazy promotion path and the existing small-cache semantics.

## Why

The prior small Vec cache removed HashMap overhead, but still allocated a Vec at the start of every graphics flush frame. Many frames have zero or one cacheable background-preflight role/instance. Storing the first result separately avoids allocation on those common low-cardinality frames while preserving reuse for repeated role/instance requests and preserving the multi-entry cache behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_1d7a8e"` — `tj-a0ba75f6`, passed
