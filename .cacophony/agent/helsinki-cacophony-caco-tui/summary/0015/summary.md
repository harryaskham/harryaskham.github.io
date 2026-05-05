# bd-6a4f8b: avoid duplicate text-decoration cache lookups in TUI graphics flush

## What changed

- Updated `App::flush_graphics_requests()` text-decoration paths to use `TextDecorationRenderer::render_with_stats_ref()` for:
  - span glows
  - span pills
  - flat sidebar/header decorations
  - cursor glows
- Removed the previous pattern where the app first called `stats_for_render()` and then called `render()` with the same params.
- Added regression coverage that the app flush path no longer contains the duplicate decoration probe.

## Why

Decoration surfaces are part of the graphics hot path. The old pattern rebuilt and hash-looked-up the same text-decoration cache key twice per decoration on steady frames: once for telemetry and once for retrieving the PNG `Arc`. `render_with_stats_ref()` returns the PNG and cache hit/miss counters from one lookup, preserving output and telemetry while trimming unnecessary CPU/hash work.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_6a4f8b"` — `tj-92f73590`, passed
