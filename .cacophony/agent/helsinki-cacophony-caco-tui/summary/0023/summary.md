# bd-759a5d: count unchanged border fast-path hits in benchmark telemetry

## What changed

- `BorderIntegration::register_panel()` now reports cache hits when an unchanged panel reuses already-live border segment surfaces.
- Title/header decoration fast-path reuse is counted as decoration cache hits when present.
- Added regression coverage for unchanged panel fast-path hit accounting.

## Why

The fastest border path is when an unchanged panel simply marks its existing kitty surfaces live and returns without invoking the renderer. That is correct for performance, but returning zero stats made real-dashboard benchmark `border_cache_hit_rate` under-report retained border reuse. Counting these fast-path reuses makes benchmark telemetry less misleading without changing rendering or surface lifecycle behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/border_integration.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_759a5d"` — `tj-a65c8762`, passed
