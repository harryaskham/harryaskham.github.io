# Session summary — per-scene render-only timing in benchmark JSON

## Goal

No ready TUI-performance bead was on the board, so per the tui-animation-optimiser
loop I ran the exploration pass and found a concrete observability gap rather than
idling: the real-TUI benchmark JSON exposed render-only (app/draw) timing tails at
the top level but not per scene, even though per-scene upload-pass and terminal-sync
timing tails were already present. I added per-scene render-only timing so future
optimizers can attribute app/render-side cost to a specific scene without inferring
it from work-frame minus upload/terminal cost.

## Bead(s)

- `bd-fcd248` — Benchmark JSON: add per-scene render-only timing tails
- (tracker: `bd-5b25e0` — Permanent TUI animation optimisation tracker)

## Before state

- Failing tests: none
- `RealTuiSceneSummary` exposed per-scene upload-pass (avg/p95/p99/max/slow),
  terminal-sync (avg/p95/p99/max/slow), terminal-inclusive work-frame, and frame
  (median/p95/p99) timing, but no per-scene render-only timing.
- Top-level result already reported avg/p95/p99/max render-only.

## After state

- Failing tests: none. `cargo test -p caco-tui --lib benchmark_support` = 33 passed.
- `RealTuiSceneSummary` now also exposes `avg_render_only_ms`,
  `p95_render_only_ms`, `p99_render_only_ms`, `max_render_only_ms`.
- Per-scene render-only timing is accumulated through `BenchmarkSceneIoStats`
  exactly like per-scene upload-pass timing (new `render_only_us`,
  `render_only_count`, `render_only_times`, `max_render_only_us`), populated from
  the existing per-frame `render_time` in the benchmark loop. No
  `build_real_benchmark_result` signature change, so the ~12 call sites were
  untouched.

## Diff summary

- Code/content commit: pending final squash SHA from reintegration receipt.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`
- Tests: +1 (`real_benchmark_scene_summary_reports_render_only_timing_bd_fcd248`)
- Behavioural delta: benchmark JSON gains four per-scene fields; rendering, upload,
  and timing behavior are unchanged. This is an observability-only change, validated
  in text mode (no actual-Kitty FPS claim — this agent runs on a microvm without an
  Xvfb/kitty graphics surface).

## Operator-takeaway

Per-scene render-only timing closes the last app/render-side attribution gap in the
benchmark schema: scene summaries already isolated upload-pass and terminal-sync
cost, and now isolate the draw/render cost too, so a regression in one scene's
render path can be pinpointed from JSON without raw frame traces.
