# Session summary — Expose per-scene graphics activity rate

## Goal

Continue the caco-tui optimiser loop with a safe benchmark-observability slice: make scene-local graphics activity directly visible before interpreting per-scene upload, byte, delete, cache, or terminal-sync densities.

## Bead(s)

- `bd-15b56b` — Expose per-scene graphics activity rate in TUI benchmark JSON.

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: actual Xvfb/kitty real-TUI baseline on main `70e9f403d`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, app-side work FPS ≈432.0, terminal-inclusive work FPS ≈178.3, and `frames_with_graphics=620` of `total_frames=837`. Scene summaries exposed raw `frames_with_graphics`, but not a scene-local rate.
- Context: scene frame counts differ in cycling benchmarks, so raw graphics-frame counts still required manual division before comparing how much of each scene exercised graphics.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: a short actual Xvfb/kitty after-run confirmed debug scene JSON now includes per-scene `graphics_frame_rate`. Example after-run values included `overview_agents graphics_frame_rate=1.0` and `project_beads_board graphics_frame_rate≈0.698`. The after-run had the expected dirty-source caveat because the benchmark binary embeds the prior committed SHA before this local commit.
- Context: this is observability-only. Rendering behavior, cache decisions, upload batching, cleanup scheduling, and terminal output are unchanged.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: extended the benchmark-support scene summary test to cover per-scene graphics activity rate.
- Behavioural delta: real-TUI benchmark debug scene JSON now exposes `graphics_frame_rate` per scene; no live TUI rendering behavior changes.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui real_benchmark_result_reports_scene_cache_upload_and_terminal_timing_bd_b613a1_bd_0b861e_bd_76efd8_bd_8ca74c_bd_60c4f1_bd_b31ffa_bd_686d57_bd_15b56b`; `cargo test -p caco-tui app::benchmark_support::tests`; actual `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 3 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`; `cargo clippy -p caco-tui --lib -- -D warnings`; full `cargo test -p caco-tui`; `git diff --check`.

## Operator-takeaway

Scene summaries now normalize graphics-active frame counts by local frame count. Future graphics work can interpret scene-local upload/cache/terminal metrics with immediate context about how much of that scene actually exercised graphics.
