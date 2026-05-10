# Session summary — Expose p99 upload-pass timing

## Goal

Continue the caco-tui optimiser loop with a safe benchmark-observability slice: make high-tail upload-pass timing visible in benchmark JSON before attempting risky graphics upload, cache, or output-size changes.

## Bead(s)

- `bd-30c253` — Expose p99 upload-pass timing in TUI benchmark JSON.

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: actual Xvfb/kitty real-TUI baseline on main `c04635e03`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, app-side work FPS ≈466.3, terminal-inclusive work FPS ≈186.3, top-level `p95_upload_pass_ms≈0.0026`. Benchmark JSON exposed p95, max, and slow-frame upload-pass timing, but not p99 upload-pass timing.
- Context: p99 upload-pass timing helps distinguish high-tail upload-pass cost from isolated max spikes without raw frame traces.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: a short actual Xvfb/kitty after-run confirmed debug JSON now includes top-level and per-scene `p99_upload_pass_ms`. Example after-run values included top-level `p99_upload_pass_ms≈0.0043`, `overview_agents p99_upload_pass_ms≈0.004`, and `project_beads_board p99_upload_pass_ms≈0.002`. The after-run had the expected dirty-source caveat because the benchmark binary embeds the prior committed SHA before this local commit.
- Context: this is observability-only. Rendering behavior, cache decisions, upload batching, cleanup scheduling, and terminal output are unchanged.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: extended the benchmark-support scene summary test to cover top-level and per-scene p99 upload-pass timing.
- Behavioural delta: real-TUI benchmark JSON now exposes `p99_upload_pass_ms` overall and per debug scene; no live TUI rendering behavior changes.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui real_benchmark_result_reports_scene_cache_upload_and_terminal_timing_bd_b613a1_bd_0b861e_bd_76efd8_bd_8ca74c_bd_60c4f1_bd_b31ffa_bd_686d57_bd_15b56b_bd_91b988_bd_b1cf5e_bd_ddc9bd_bd_79fa4e_bd_30c253`; `cargo test -p caco-tui app::benchmark_support::tests`; actual `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 3 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`; `cargo clippy -p caco-tui --lib -- -D warnings`; full `cargo test -p caco-tui`; `git diff --check`.

## Operator-takeaway

Benchmark JSON now includes p99 upload-pass timing alongside p95, max, and slow-frame counts. Future graphics work can identify high-tail upload cost without relying on max-only inference or raw traces.
