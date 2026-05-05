# bd-c201b3: TUI benchmark headroom truthfulness

## What changed

- Added `--uncapped` to `caco tui dashboard-benchmark`, `caco tui benchmark`, and `caco tui fps-benchmark --real`.
- Real-dashboard benchmark JSON now reports both paced target-FPS results and headroom/work metrics:
  - `pacing_mode`
  - `work_secs`
  - `work_fps`
  - `avg_work_frame_ms`
- Updated `scripts/tui-fps-bench.sh` to pass/report `--uncapped` and annotate harness metadata with `uncapped`.
- Updated `SPEC.md`, `README.md`, `AGENTS.md`, and `docs/tui.html` so future TUI perf audits use the real dashboard plus uncapped mode when comparing Kitty graphics to text.

## Evidence

Validation:

- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs crates/caco-cli/src/lib.rs`
- `bash -n scripts/tui-fps-bench.sh`
- `git diff --check`
- `docs/validate-pages.sh`
- `caco test run --wait --command "cargo test -p caco-tui real_benchmark_result_reports_"` — `tj-f3872fe0`, passed
- `caco test run --wait --command "cargo check -p caco-cli"` — `tj-45c5e351`, passed
- `caco build run --wait --command "cargo build -p caco"` — `bj-434cdc93`, succeeded

Headless Xvfb + Kitty, same terminal, uncapped dev-binary sample:

- Text path in Kitty terminal: `work_fps=137.0`, `avg_work_frame_ms=7.30`, `uploads=0`, `deletes=0`
- Graphics path in Kitty terminal: `work_fps=123.2`, `avg_work_frame_ms=8.11`, `uploads=108`, `deletes=122`, `upload_wire_bytes=1,682,378`

This proves the old parity-looking target-FPS number was not sufficient evidence: the benchmark now exposes real work/headroom and upload churn directly. In this local sample, graphics is slower but not 10× slower; future operator repros can compare `work_fps`/`avg_work_frame_ms` under the same terminal path without target pacing hiding the difference.
