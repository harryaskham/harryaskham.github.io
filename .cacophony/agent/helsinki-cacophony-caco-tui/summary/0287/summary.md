# Session summary — Expose per-scene max upload-pass timing

## Goal

Continue the caco-tui optimiser loop with another safe benchmark-observability improvement: expose scene-local worst upload-pass spikes so graphics investigations can separate steady upload cost from intermittent stalls before changing rendering code.

## Bead(s)

- `bd-7777bd` — Expose per-scene max upload-pass timing in TUI benchmark JSON.

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: actual Xvfb/kitty real-TUI baseline on main `92f058f1d`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈430.9, terminal-inclusive work FPS ≈179.6, avg work ≈2.32ms, avg terminal-inclusive ≈5.57ms, avg upload pass ≈0.70ms. Scenes: `overview_agents` ≈762.6 work FPS / avg ≈1.31ms / avg upload pass ≈0.001ms, `project_beads_board` ≈362.1 / avg ≈2.76ms / avg upload pass ≈0.94ms, `feed_logs` ≈425.5 / avg ≈2.35ms / avg upload pass ≈0.78ms.
- Context: after `bd-0b861e`, scene summaries included average upload-pass timing, but not a max/worst-frame timing. That hid intermittent scene-local upload stalls unless an operator captured raw frame traces.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: a short actual Xvfb/kitty after-run confirmed debug scene JSON now includes `max_upload_pass_ms`. Example values: `overview_agents` reported `avg_upload_pass_ms≈0.001` and `max_upload_pass_ms≈0.017` with no uploads; `project_beads_board` reported `avg_upload_pass_ms≈0.84` and `max_upload_pass_ms≈310.8` with 108 uploads and ≈13.8MB estimated wire traffic. The after-run had the usual dirty-source caveat because the benchmark binary embeds the prior commit SHA before this commit.
- Context: this is observability-only. Rendering behavior, cache decisions, upload batching, and terminal output are unchanged.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: extended the benchmark-support scene summary test to cover `max_upload_pass_ms`.
- Behavioural delta: real-TUI benchmark debug scene JSON now exposes max upload-pass time per scene.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui real_benchmark_result_reports_scene_cache_rates_and_upload_pass_bd_b613a1_bd_0b861e`; `cargo test -p caco-tui app::benchmark_support::tests`; actual `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 3 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`; `cargo clippy -p caco-tui --lib -- -D warnings`; full `cargo test -p caco-tui`; `git diff --check`.

## Operator-takeaway

Benchmark scene summaries now show both average and max upload-pass timing, making it much easier to spot whether a scene has steady graphics cost or rare upload spikes before attempting any risky cache/rendering change.
