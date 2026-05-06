# bd-041aac: avoid background preflight cache key clone on hit

## What changed

- Updated the per-frame background preflight cache lookup in `App::flush_graphics_requests()` to compare cached entries against `request.instance.as_deref()` before cloning.
- The request instance `String` is now cloned only when inserting a cache miss.
- Repeated role/instance cache hits avoid per-request `Option<String>` cloning.
- Background image override requests still bypass the role/instance cache and force background handling.
- Added source-shape coverage for borrowed instance comparison and clone-on-miss insertion.

## Why

The small Vec cache removed HashMap allocation, but the lookup still cloned `request.instance` before checking whether an entry already existed. On common cache-hit frames with repeated role/instance panels, that clone was pure overhead. Borrowed comparison preserves the same cache semantics with fewer allocations.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- Initial `caco test run --wait --command "cargo test -p caco-tui bd_041aac"` — `tj-198ef0d7`, failed due rustfmt line wrapping in the source assertion.
- Adjusted assertion to match formatted source.
- Rerun `caco test run --wait --command "cargo test -p caco-tui bd_041aac"` — `tj-2109f48f`, passed
