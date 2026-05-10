# Session summary — Expose per-scene upload-pass timing

## Goal

Continue the caco-tui optimiser loop with a safe benchmark-observability improvement: expose which real-TUI benchmark scene is paying upload-pass time, without changing rendering or upload behavior.

## Bead(s)

- `bd-0b861e` — Expose per-scene upload-pass timing in TUI benchmark JSON.

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: actual Xvfb/kitty real-TUI baseline on main `f8fce4e46`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈473.5, terminal-inclusive work FPS ≈185.7, avg work ≈2.11ms, avg terminal-inclusive ≈5.38ms, avg upload pass ≈0.65ms. Scenes: `overview_agents` ≈919.6 work FPS / avg ≈1.09ms, `project_beads_board` ≈395.8 / avg ≈2.53ms, `feed_logs` ≈451.6 / avg ≈2.21ms.
- Context: after `bd-b613a1`, scene summaries included cache rates and raw upload counts/bytes, but not per-scene upload-pass timing. That meant graphics investigations could see which scene uploaded bytes, but not the measured upload-pass time attributable to each scene.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: a short actual Xvfb/kitty after-run confirmed debug scene JSON now includes `avg_upload_pass_ms`. Example values: `overview_agents` reported `avg_upload_pass_ms≈0.0026` with no uploads, while `project_beads_board` reported `avg_upload_pass_ms≈0.85` with 108 uploads and ≈13.8MB estimated wire traffic. The after-run had the usual dirty-source caveat because the benchmark binary embeds the prior commit SHA before this commit.
- Context: this is observability-only. Rendering behavior, cache decisions, upload batching, and terminal output are unchanged.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: extended the benchmark-support scene summary test to cover `avg_upload_pass_ms`.
- Behavioural delta: real-TUI benchmark debug scene JSON now exposes average upload-pass time per scene.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui real_benchmark_result_reports_scene_cache_rates_and_upload_pass_bd_b613a1_bd_0b861e`; `cargo test -p caco-tui app::benchmark_support::tests`; actual `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 3 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`; `cargo clippy -p caco-tui --lib -- -D warnings`; full `cargo test -p caco-tui`; `git diff --check`.

## Operator-takeaway

Future graphics-cache or upload investigations can now compare per-scene cache rates, upload bytes, and upload-pass timing directly in benchmark JSON before making any risky rendering changes.
