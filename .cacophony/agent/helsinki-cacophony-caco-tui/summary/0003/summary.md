# bd-dcab76: benchmark animation phase parity audit

## What changed

- The real-dashboard benchmark no longer calls `SurfaceManager::advance_animation_frame()` unconditionally every benchmark loop iteration.
- Instead it routes through the same `TuiEvent::AnimationFrame` handler as the live TUI, preserving the quantized `border_integration.animation_phase_would_advance()` gate.
- This matters especially for `--uncapped`: a loop running faster than the configured visual phase cadence should measure real dashboard render/headroom, not manufacture a bitmap animation redraw on every iteration.
- Updated `SPEC.md` to require benchmark animation behavior to match the live TUI event path.

## Evidence

Validation:

- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui real_benchmark_result_reports_"` — `tj-9cf7ca06`, passed
- `caco build run --wait --command "cargo build -p caco"` — `bj-50c67a8d`, succeeded

Same-terminal Xvfb + Kitty uncapped dev-binary sample after the benchmark phase-gate change:

- Text path in Kitty terminal: `work_fps=197.9`, `avg_work_frame_ms=5.05`, `uploads=0`, `deletes=0`
- Graphics path in Kitty terminal: `work_fps=112.7`, `avg_work_frame_ms=8.87`, `uploads=226`, `deletes=230`, `upload_wire_bytes=3,525,250`

The remaining overhead is now visible as real scene-transition graphics work, not hidden by target-FPS pacing and not exaggerated by forcing every uncapped frame into an animation redraw. This gives the next optimization slice a truthful target: reduce initial/transition upload/delete volume and render cost while keeping stale placement cleanup correct.
