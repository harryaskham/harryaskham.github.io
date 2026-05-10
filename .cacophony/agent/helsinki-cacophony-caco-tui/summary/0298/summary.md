# Session summary — Expose per-scene p99 terminal-sync timing

## Goal

Continue the caco-tui optimiser loop with a safe benchmark-observability slice: make scene-local high-tail terminal-sync timing visible without raw frame traces before attempting risky terminal-output or upload-cadence changes.

## Bead(s)

- `bd-79fa4e` — Expose per-scene p99 terminal-sync timing in TUI benchmark JSON.

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: actual Xvfb/kitty real-TUI baseline on main `d184f7271`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, app-side work FPS ≈395.0, terminal-inclusive work FPS ≈174.3, top-level `p95_terminal_sync_ms≈3.25`. Scene summaries exposed average, p95, max, and slow-frame terminal-sync timing, but not p99 terminal-sync timing.
- Context: scene-local p99 terminal-sync timing helps distinguish high-tail terminal-side processing cost by scene without inferring from whole-run percentiles or digging through raw frame traces.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: a short actual Xvfb/kitty after-run confirmed debug scene JSON now includes per-scene `p99_terminal_sync_ms`. Example after-run values included `overview_agents p99_terminal_sync_ms≈3.233` and `project_beads_board p99_terminal_sync_ms≈3.575`. The after-run had the expected dirty-source caveat because the benchmark binary embeds the prior committed SHA before this local commit.
- Context: this is observability-only. Rendering behavior, cache decisions, upload batching, cleanup scheduling, and terminal output are unchanged.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: extended the benchmark-support scene summary test to cover per-scene p99 terminal-sync timing.
- Behavioural delta: real-TUI benchmark debug scene JSON now exposes `p99_terminal_sync_ms` per scene; no live TUI rendering behavior changes.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui real_benchmark_result_reports_scene_cache_upload_and_terminal_timing_bd_b613a1_bd_0b861e_bd_76efd8_bd_8ca74c_bd_60c4f1_bd_b31ffa_bd_686d57_bd_15b56b_bd_91b988_bd_b1cf5e_bd_ddc9bd_bd_79fa4e`; `cargo test -p caco-tui app::benchmark_support::tests`; actual `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 3 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`; `cargo clippy -p caco-tui --lib -- -D warnings`; full `cargo test -p caco-tui`; `git diff --check`.

## Operator-takeaway

Scene summaries now include average, p95, p99, max, and slow-frame terminal-sync timing. Future graphics work can identify scene-local high-tail terminal-side cost without relying on whole-run percentiles or raw traces.
