# bd-839113: effective graphics benchmark reporting

## What changed

- Real-dashboard benchmark JSON now distinguishes terminal support from the actual enabled graphics path:
  - `graphics_capability` remains the detected terminal protocol (for example `Kitty`).
  - `graphics_effective_enabled` reports whether the TUI actually used graphics after config/effective enablement.
- `scripts/tui-fps-bench.sh --require-graphics` now rejects runs where the terminal is Kitty/Ghostty but effective graphics are disabled.
- The script summary line now prints `capability=<...> effective=<true|false>` without treating `false` as missing.
- Updated `SPEC.md`, `README.md`, and `docs/tui.html` so future performance evidence cannot confuse "text mode inside Kitty" with a real graphics-mode run.

## Evidence

Validation:

- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs`
- `bash -n scripts/tui-fps-bench.sh`
- `git diff --check`
- `docs/validate-pages.sh`
- `caco test run --wait --command "cargo test -p caco-tui real_benchmark_result_reports_"` — `tj-7d6d98e4`, passed
- `caco build run --wait --command "cargo build -p caco"` — `bj-257f1e1b`, succeeded

Headless Xvfb + Kitty sample with the updated script/binary:

- Text path inside Kitty (`tui.graphics.enabled: false`): JSON `graphics_capability=Kitty`, `graphics_effective_enabled=false`, `work_fps=222.7`, `uploads=0`, `frames_with_graphics=0`.
- `--require-graphics` correctly rejected that effective-disabled run with `capability=Kitty, effective=false`.
- Forced graphics path: JSON `graphics_capability=Kitty`, `graphics_effective_enabled=true`, `work_fps=105.7`, `uploads=108`, `frames_with_graphics=2`.

This fixes a benchmark-evidence footgun: the terminal capability alone no longer makes a text run look like a graphics run.
