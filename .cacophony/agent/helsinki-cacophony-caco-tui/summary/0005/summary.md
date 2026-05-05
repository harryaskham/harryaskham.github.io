# bd-ed3b98: scene-local real TUI benchmark FPS math

## What changed

- Fixed real-dashboard benchmark debug scene summaries to use scene-local frame/work denominators instead of dividing each scene's frame count by the whole measurement window.
- `RealTuiSceneSummary` now exposes:
  - `work_secs`
  - `work_fps`
  - `avg_work_frame_ms`
- Kept `avg_fps` for compatibility, but made it reflect the scene-local work/headroom FPS.
- Updated `SPEC.md` and `docs/tui.html` so future graphics-vs-text hotspot audits require scene-local work metrics.

## Why

Graphics overhead often appears during specific scene transitions or views. The old per-scene `avg_fps` denominator diluted short scenes across the entire benchmark duration, which made per-view graphics/text comparisons misleading. This change keeps the benchmark evidence useful for finding the next border/background upload or render hotspot.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs`
- `git diff --check`
- `docs/validate-pages.sh`
- `caco test run --wait --command "cargo test -p caco-tui bd_ed3b98"` — `tj-af0b60a3`, passed
- `caco test run --wait --command "cargo test -p caco-tui real_benchmark_result_reports_"` — `tj-add48364`, passed
