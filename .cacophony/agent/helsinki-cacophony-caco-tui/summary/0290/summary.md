# Session summary — Expose per-scene terminal-sync tail metrics

## Goal

Continue the caco-tui optimiser loop with a safe benchmark-observability slice: after adding per-scene average terminal-sync timing, expose scene-local terminal-sync tail metrics so future graphics work can identify whether a scene has steady terminal overhead or occasional terminal-side spikes.

## Bead(s)

- `bd-8ca74c` — Expose per-scene terminal-sync tail metrics in TUI benchmark JSON.

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: actual Xvfb/kitty real-TUI baseline on main `d9ee3c302`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, app-side work FPS ≈367.9, terminal-inclusive work FPS ≈166.9, avg work ≈2.72ms, avg terminal-inclusive ≈5.99ms, avg terminal sync ≈3.27ms. Per-scene averages showed terminal sync around 3.2–3.3ms, but scene summaries did not expose max terminal-sync cost or count slow terminal-sync frames.
- Context: the benchmark could show top-level p95 terminal sync and per-scene average terminal sync, but future optimizers still had to infer whether terminal-side cost was steady or spiky by scene.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: a short actual Xvfb/kitty after-run confirmed debug scene JSON now includes per-scene `max_terminal_sync_ms` and `terminal_sync_slow_frames` alongside the existing per-scene average terminal sync and terminal-inclusive FPS. Example after-run values included `overview_agents max_terminal_sync_ms≈3.51, terminal_sync_slow_frames=0` and `project_beads_board max_terminal_sync_ms≈25.65, terminal_sync_slow_frames=1`. The after-run had the expected dirty-source caveat because the benchmark binary embeds the prior committed SHA before this local commit.
- Context: this is observability-only. Rendering behavior, cache decisions, upload batching, and terminal output are unchanged.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: extended the benchmark-support scene summary test to cover max terminal-sync timing and slow terminal-sync frame counts, using the same 16ms threshold as the upload slow-frame metric.
- Behavioural delta: real-TUI benchmark debug scene JSON now exposes terminal-sync tail metrics per scene; no live TUI rendering behavior changes.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui real_benchmark_result_reports_scene_cache_upload_and_terminal_timing_bd_b613a1_bd_0b861e_bd_76efd8_bd_8ca74c`; `cargo test -p caco-tui app::benchmark_support::tests`; actual `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 3 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`; `cargo clippy -p caco-tui --lib -- -D warnings`; full `cargo test -p caco-tui`; `git diff --check`.

## Operator-takeaway

Scene summaries can now show whether terminal-side graphics cost is steady or has tail spikes. That makes future terminal-output/upload cadence work easier to target without relying on whole-run p95 data or raw frame traces.
