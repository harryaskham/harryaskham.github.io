# Session summary — Expose per-scene p99 frame timing

## Goal

Continue the caco-tui optimiser loop with a safe benchmark-observability slice: make scene-local high-tail app frame timing visible without raw frame traces before attempting risky render, upload, or layout changes.

## Bead(s)

- `bd-ddc9bd` — Expose per-scene p99 frame timing in TUI benchmark JSON.

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: actual Xvfb/kitty real-TUI baseline on main `b516cf468`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, app-side work FPS ≈424.8, terminal-inclusive work FPS ≈179.2, top-level `p99_frame_ms≈2.46`. Scene summaries exposed median and p95 frame timing, but not p99 frame timing.
- Context: scene-local p99 frame timing helps distinguish high-tail app/render cost by scene without inferring from whole-run p99 or digging through raw frame traces.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: a short actual Xvfb/kitty after-run confirmed debug scene JSON now includes per-scene `p99_frame_ms`. Example after-run values included `overview_agents p99_frame_ms≈1.62` and `project_beads_board p99_frame_ms≈2.09`. The after-run had the expected dirty-source caveat because the benchmark binary embeds the prior committed SHA before this local commit.
- Context: this is observability-only. Rendering behavior, cache decisions, upload batching, cleanup scheduling, and terminal output are unchanged.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: extended the benchmark-support scene summary test to cover per-scene p99 frame timing.
- Behavioural delta: real-TUI benchmark debug scene JSON now exposes `p99_frame_ms` per scene; no live TUI rendering behavior changes.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui real_benchmark_result_reports_scene_cache_upload_and_terminal_timing_bd_b613a1_bd_0b861e_bd_76efd8_bd_8ca74c_bd_60c4f1_bd_b31ffa_bd_686d57_bd_15b56b_bd_91b988_bd_b1cf5e_bd_ddc9bd`; `cargo test -p caco-tui app::benchmark_support::tests`; actual `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 3 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`; `cargo clippy -p caco-tui --lib -- -D warnings`; full `cargo test -p caco-tui`; `git diff --check`.

## Operator-takeaway

Scene summaries now include median, p95, and p99 frame timing. Future graphics or layout work can identify scene-local high-tail app-frame cost without relying on whole-run percentiles or raw traces.
