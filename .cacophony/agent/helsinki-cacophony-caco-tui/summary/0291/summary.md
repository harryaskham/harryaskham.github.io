# Session summary — Expose per-scene upload density

## Goal

Continue the caco-tui optimiser loop with a safe benchmark-observability slice: make scene-local upload churn directly comparable across uneven scene frame counts before attempting risky graphics upload or cache changes.

## Bead(s)

- `bd-60c4f1` — Expose per-scene upload density in TUI benchmark JSON.

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: actual Xvfb/kitty real-TUI baseline on main `de7f12c79`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, app-side work FPS ≈403.7, terminal-inclusive work FPS ≈173.9, avg terminal sync ≈3.27ms, top-level `uploads_per_frame≈0.276`, and `retained_redisplays_per_frame=0`. Scene summaries exposed raw `uploads_succeeded` and `retained_redisplays`, but not per-frame densities.
- Context: scene frame counts differ substantially in cycling benchmarks, so raw upload counts require manual normalization before comparing scene-local churn.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: a short actual Xvfb/kitty after-run confirmed debug scene JSON now includes per-scene `uploads_per_frame` and `retained_redisplays_per_frame`. Example after-run values included `overview_agents uploads_per_frame=0.0`, `retained_redisplays_per_frame=0.0`; `project_beads_board uploads_per_frame≈0.303`, `retained_redisplays_per_frame=0.0`. The after-run had the expected dirty-source caveat because the benchmark binary embeds the prior committed SHA before this local commit.
- Context: this is observability-only. Rendering behavior, cache decisions, upload batching, and terminal output are unchanged.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: extended the benchmark-support scene summary test to cover per-scene upload and retained-redisplay densities.
- Behavioural delta: real-TUI benchmark debug scene JSON now exposes upload and retained-redisplay density per scene; no live TUI rendering behavior changes.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui real_benchmark_result_reports_scene_cache_upload_and_terminal_timing_bd_b613a1_bd_0b861e_bd_76efd8_bd_8ca74c_bd_60c4f1`; `cargo test -p caco-tui app::benchmark_support::tests`; actual `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 3 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`; `cargo clippy -p caco-tui --lib -- -D warnings`; full `cargo test -p caco-tui`; `git diff --check`.

## Operator-takeaway

Scene summaries now normalize upload and retained-redisplay counts by local frame count. Future graphics work can compare upload churn across scenes directly instead of doing manual arithmetic on raw counts from uneven benchmark slices.
