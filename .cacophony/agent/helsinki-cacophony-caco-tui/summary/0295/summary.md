# Session summary — Expose per-scene p95 upload-pass timing

## Goal

Continue the caco-tui optimiser loop with a safe benchmark-observability slice: make scene-local upload-pass tail timing visible without raw frame traces before attempting risky graphics upload or cache changes.

## Bead(s)

- `bd-91b988` — Expose per-scene p95 upload-pass timing in TUI benchmark JSON.

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: actual Xvfb/kitty real-TUI baseline on main `778c544d2`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, app-side work FPS ≈406.1, terminal-inclusive work FPS ≈174.9, top-level `p95_upload_pass_ms≈0.0029`. Scene summaries exposed average upload-pass timing, max upload-pass timing, and slow-frame counts, but not p95 upload-pass timing.
- Context: per-scene max upload timing identifies one-frame spikes while average timing hides tails; p95 bridges that gap for scene-local steady-tail upload cost.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: a short actual Xvfb/kitty after-run confirmed debug scene JSON now includes per-scene `p95_upload_pass_ms`. Example after-run values included `overview_agents p95_upload_pass_ms≈0.002` and `project_beads_board p95_upload_pass_ms≈0.003`. The after-run had the expected dirty-source caveat because the benchmark binary embeds the prior committed SHA before this local commit.
- Context: this is observability-only. Rendering behavior, cache decisions, upload batching, cleanup scheduling, and terminal output are unchanged.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: extended the benchmark-support scene summary test to cover per-scene p95 upload-pass timing.
- Behavioural delta: real-TUI benchmark debug scene JSON now exposes `p95_upload_pass_ms` per scene; no live TUI rendering behavior changes.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui real_benchmark_result_reports_scene_cache_upload_and_terminal_timing_bd_b613a1_bd_0b861e_bd_76efd8_bd_8ca74c_bd_60c4f1_bd_b31ffa_bd_686d57_bd_15b56b_bd_91b988`; `cargo test -p caco-tui app::benchmark_support::tests`; actual `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 3 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`; `cargo clippy -p caco-tui --lib -- -D warnings`; full `cargo test -p caco-tui`; `git diff --check`.

## Operator-takeaway

Scene summaries now include average, p95, max, and slow-frame upload-pass timing. Future graphics work can distinguish steady-tail upload cost from isolated spikes by scene without digging through raw frame traces.
