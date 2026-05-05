# bd-4e32e5: default-on real TUI graphics cache telemetry

## What changed

- Added `App::detailed_graphics_perf_enabled()` so absent/default graphics config follows the documented default `detailedPerfTelemetry=true`.
- `flush_graphics_requests()` and `render_graphics_background()` now use that resolver instead of treating `graphics_config: None` as telemetry disabled.
- Updated `SPEC.md` and `docs/tui.html` to state that detailed graphics cache/traffic telemetry is default-on unless `detailedPerfTelemetry: false` is explicit.

## Why

The real-dashboard benchmark can run from built-in/default TUI config with no explicit `tui.graphics` object. Before this fix, that path produced misleading zero cache hit rates even though caches were active. That made graphics-vs-text evidence less useful when looking for border/background hotspots.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `docs/validate-pages.sh`
- `caco test run --wait --command "cargo test -p caco-tui bd_4e32e5"` — `tj-eee77619`, passed
