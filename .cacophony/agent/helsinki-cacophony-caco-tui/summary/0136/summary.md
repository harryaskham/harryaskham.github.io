# bd-21d08b: skip background preflight when graphics config/cache absent

## What changed

- Added an outer `graphics_config.is_some() || active_background_roots.is_some()` guard in `App::flush_graphics_requests()` before running the background preflight helper.
- No-config/no-background-cache graphics frames now skip background preflight entirely for each border request.
- Existing background cache state still forces the preflight/resolution path so stale cached background roots can be pruned/removed correctly.
- Added source-shape coverage for the cheap config/cache guard.

## Why

The recent raw-config preflight avoids expensive layer resolution for disabled backgrounds, but on sessions with no graphics config and no prior background cache, even that preflight is avoidable. This removes another per-panel no-op branch from graphics-capable frames while retaining cleanup correctness when background cache state exists.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_21d08b"` — `tj-1b8944b8`, passed
