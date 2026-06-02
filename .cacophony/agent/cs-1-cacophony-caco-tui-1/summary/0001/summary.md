# Session summary — render-only slow-frame counters in benchmark JSON

## Goal

Continuing the tui-animation-optimiser exploration loop after a crash revival
with no ready TUI-performance bead, I closed a benchmark-schema symmetry gap:
render-only (app/draw) timing was the only timing phase with no slow-frame
counter. Upload-pass and terminal-sync each expose a `*_slow_frames` (>=16ms)
count at both the top level and per scene, but render-only only exposed
avg/p95/p99/max. That meant app-render stalls — frames whose draw cost alone
blew the 16ms interactive budget — were uncountable from the JSON without raw
frame traces. This session adds the missing counter symmetrically.

## Bead(s)

- `bd-082182` — Benchmark JSON: add render-only slow-frame counters (top-level + per-scene)
- (tracker: `bd-5b25e0` — Permanent TUI animation optimisation tracker)

## Before state

- Failing tests: none.
- Top-level `RealTuiBenchmarkResult` had `upload_pass_slow_frames` and
  `terminal_sync_slow_frames` but no `render_only_slow_frames`.
- Per-scene `RealTuiSceneSummary` had `upload_pass_slow_frames` and
  `terminal_sync_slow_frames` (plus the render-only avg/p95/p99/max added last
  session in bd-fcd248) but no render-only slow-frame count.

## After state

- Failing tests: none. `cargo test -p caco-tui --lib benchmark_support` = 33 passed.
  `cargo clippy -p caco-tui --lib` clean (only a pre-existing `type_complexity`
  warning in the caco-daemon dependency).
- `render_only_slow_frames` now exists at the top level (counted from
  `render_times` >= 16ms) and per scene (accumulated through
  `BenchmarkSceneIoStats`, incremented when a frame's render-only duration is
  >=16ms), mirroring the upload-pass and terminal-sync slow-frame counters
  exactly.

## Diff summary

- Code/content commit: pending final squash SHA from reintegration receipt.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`
- Tests: +0 new test functions; extended
  `real_benchmark_scene_summary_reports_render_only_timing_bd_fcd248` to assert
  both the per-scene and headline `render_only_slow_frames` counters (the
  existing 20ms sample frame already crosses the 16ms budget).
- Behavioural delta: benchmark JSON gains one top-level and one per-scene field;
  rendering, upload, and timing behavior unchanged. Observability-only, validated
  in text mode (this microvm has no Xvfb/kitty graphics surface).

## Operator-takeaway

The benchmark schema's three timing phases (render-only, upload-pass,
terminal-sync) now all expose the same avg/p95/p99/max + slow-frame shape, so an
optimizer can ask "which phase blew the 16ms budget, and how often" uniformly
without special-casing render-only or dropping to raw frame traces.
