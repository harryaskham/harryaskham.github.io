# Session summary — Expose per-scene terminal-sync timing

## Goal

Continue the caco-tui optimiser loop with a safe benchmark-observability improvement: show terminal-inclusive scene costs directly so future graphics work can distinguish app/render work from terminal processing overhead before touching rendering code.

## Bead(s)

- `bd-76efd8` — Expose per-scene terminal-sync timing in TUI benchmark JSON.

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: actual Xvfb/kitty real-TUI baseline on main `2306b7478`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, app-side work FPS ≈422.3, terminal-inclusive work FPS ≈178.5, avg work ≈2.37ms, avg terminal-inclusive ≈5.60ms, avg terminal sync ≈3.23ms. Scenes already exposed app-side work FPS, cache rates, and upload timing, but not per-scene terminal-sync or terminal-inclusive FPS.
- Context: top-level benchmark output repeatedly shows terminal-inclusive FPS materially below app-side work FPS, but scene summaries did not identify how that terminal-side cost applied by scene.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: a short actual Xvfb/kitty after-run confirmed debug scene JSON now includes per-scene `terminal_inclusive_work_fps`, `avg_terminal_inclusive_work_frame_ms`, and `avg_terminal_sync_ms`. Example values: `overview_agents` reported app work FPS ≈891.9 but terminal-inclusive FPS ≈231.4 with avg terminal sync ≈3.20ms; `project_beads_board` reported app work FPS ≈510.0 but terminal-inclusive FPS ≈191.7 with avg terminal sync ≈3.26ms. The after-run had the usual dirty-source caveat because the benchmark binary embeds the prior commit SHA before this commit.
- Context: this is observability-only. Rendering behavior, cache decisions, upload batching, and terminal output are unchanged.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: extended the benchmark-support scene summary test to cover per-scene terminal-inclusive FPS, avg terminal-inclusive frame time, and avg terminal sync.
- Behavioural delta: real-TUI benchmark debug scene JSON now exposes terminal-side timing fields per scene.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui real_benchmark_result_reports_scene_cache_upload_and_terminal_timing_bd_b613a1_bd_0b861e_bd_76efd8`; `cargo test -p caco-tui app::benchmark_support::tests`; actual `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 3 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`; `cargo clippy -p caco-tui --lib -- -D warnings`; full `cargo test -p caco-tui`; `git diff --check`.

## Operator-takeaway

Benchmark scene summaries now expose both app-side and terminal-inclusive scene costs. Future optimizers can tell whether a scene is slow because of app/render work, upload pass, or terminal synchronization/processing before making risky changes.
