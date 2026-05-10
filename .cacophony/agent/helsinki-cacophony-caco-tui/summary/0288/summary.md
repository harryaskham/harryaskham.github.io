# Session summary — Expose per-scene slow upload-pass counts

## Goal

Continue the caco-tui optimiser loop with a safe benchmark-observability improvement: show how often a scene exceeds an interactive upload-pass budget, not just its average and maximum upload-pass duration.

## Bead(s)

- `bd-330449` — Expose per-scene slow upload-pass counts in TUI benchmark JSON.

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: actual Xvfb/kitty real-TUI baseline on main `d00ee1f99`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈419.5, terminal-inclusive work FPS ≈178.4, avg work ≈2.38ms, avg terminal-inclusive ≈5.60ms, avg upload pass ≈0.72ms. Scenes: `overview_agents` ≈688.7 work FPS / avg upload pass ≈0.0019ms / max ≈0.005ms, `project_beads_board` ≈355.3 / avg upload pass ≈0.97ms / max ≈313ms, `feed_logs` ≈418.1 / avg upload pass ≈0.81ms / max ≈285ms.
- Context: after `bd-7777bd`, scene summaries included average and max upload-pass times, but not how many frames crossed an interactive budget. That made it hard to distinguish one-off spikes from repeated upload stalls.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: a short actual Xvfb/kitty after-run confirmed debug scene JSON now includes `upload_pass_slow_frames`. Example values: `overview_agents` reported `slow=0` with no uploads; `project_beads_board` reported `slow=1`, `avg_upload_pass_ms≈0.79`, and `max_upload_pass_ms≈299` with 108 uploads and ≈13.8MB estimated wire traffic. The after-run had the usual dirty-source caveat because the benchmark binary embeds the prior commit SHA before this commit.
- Context: this is observability-only. Rendering behavior, cache decisions, upload batching, and terminal output are unchanged. The threshold is 16ms, roughly one 60fps frame budget.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: extended the benchmark-support scene summary test to cover `upload_pass_slow_frames`.
- Behavioural delta: real-TUI benchmark debug scene JSON now exposes the count of upload-pass frames at or above 16ms per scene.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui real_benchmark_result_reports_scene_cache_rates_and_upload_pass_bd_b613a1_bd_0b861e`; `cargo test -p caco-tui app::benchmark_support::tests`; actual `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 3 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`; `cargo clippy -p caco-tui --lib -- -D warnings`; full `cargo test -p caco-tui`; `git diff --check`.

## Operator-takeaway

Benchmark scene summaries now report whether upload cost is steady or spike-driven: `avg_upload_pass_ms`, `max_upload_pass_ms`, and `upload_pass_slow_frames` together make future graphics optimisation safer and more targeted.
