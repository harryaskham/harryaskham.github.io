# Session summary — Expose per-scene delete density

## Goal

Continue the caco-tui optimiser loop with a safe benchmark-observability slice: make scene-local kitty delete command churn directly visible before attempting risky cleanup-cadence or upload lifecycle changes.

## Bead(s)

- `bd-686d57` — Expose per-scene delete density in TUI benchmark JSON.

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: actual Xvfb/kitty real-TUI baseline on main `71ba1f30c`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, app-side work FPS ≈405.9, terminal-inclusive work FPS ≈173.9, top-level `uploads_per_frame≈0.279`, and `deletes_sent=235`. Scene summaries exposed raw `deletes_sent` and `delete_failures`, but not per-frame delete density.
- Context: scene frame counts differ in cycling benchmarks, so raw delete totals still required manual division before comparing cleanup command churn.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: a short actual Xvfb/kitty after-run confirmed debug scene JSON now includes per-scene `deletes_per_frame` and `delete_failures_per_frame`. Example after-run values included `overview_agents deletes_per_frame=0.0`, `delete_failures_per_frame=0.0`; `project_beads_board deletes_per_frame≈0.332`, `delete_failures_per_frame=0.0`. The after-run had the expected dirty-source caveat because the benchmark binary embeds the prior committed SHA before this local commit.
- Context: this is observability-only. Rendering behavior, cache decisions, upload batching, cleanup scheduling, and terminal output are unchanged.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: extended the benchmark-support scene summary test to cover per-scene delete density and delete-failure density.
- Behavioural delta: real-TUI benchmark debug scene JSON now exposes delete command density per scene; no live TUI rendering behavior changes.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui real_benchmark_result_reports_scene_cache_upload_and_terminal_timing_bd_b613a1_bd_0b861e_bd_76efd8_bd_8ca74c_bd_60c4f1_bd_b31ffa_bd_686d57`; `cargo test -p caco-tui app::benchmark_support::tests`; actual `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 3 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`; `cargo clippy -p caco-tui --lib -- -D warnings`; full `cargo test -p caco-tui`; `git diff --check`.

## Operator-takeaway

Scene summaries now normalize kitty delete command counts by local frame count. Future graphics work can compare cleanup command churn directly alongside upload counts, byte pressure, cache rates, and terminal-sync timing.
