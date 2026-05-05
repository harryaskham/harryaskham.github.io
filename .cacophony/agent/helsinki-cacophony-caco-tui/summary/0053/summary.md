# bd-0037c8: flush TUI graphics perf telemetry for retained and cache-only work

## What changed

- Expanded `GraphicsCounters::has_activity()` to include all graphics work counters, not only graphics-frame/full-upload/raster counters.
- Retained redisplays, delete/delete-failure cleanup, renderer cache hits/misses, fetch initiations, and animation-frame wakeups now keep a sampling window flushable.
- Added regression coverage for retained/cache/cleanup/fetch/animation activity and for retained-only flushes.

## Why

Recent telemetry fixes split retained Kitty redisplays and renderer-cache fast paths away from full bitmap uploads. The flush gate still only looked at full uploads, failures, raster calls, and graphics frames, so optimized retained-only or cache-only windows could be dropped before they reached the daemon. That made fast graphics paths less observable and could make benchmark/live telemetry disagree. This keeps optimized graphics activity visible without reclassifying it as full upload work.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/perf.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_0037c8"` — `tj-3d8ac053`, passed
