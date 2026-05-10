# Session summary — Expose per-scene upload byte density

## Goal

Continue the caco-tui optimiser loop with a safe benchmark-observability slice: make scene-local upload byte pressure directly visible before attempting risky graphics output-size, cache, or upload-cadence changes.

## Bead(s)

- `bd-b31ffa` — Expose per-scene upload byte density in TUI benchmark JSON.

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: actual Xvfb/kitty real-TUI baseline on main `8622fb3f9`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, app-side work FPS ≈409.9, terminal-inclusive work FPS ≈175.7, top-level `uploads_per_frame≈0.277`, and `upload_wire_bytes=26,546,596`. Scene summaries exposed raw `upload_bytes` and `upload_wire_bytes` plus upload count density, but not byte density.
- Context: scene frame counts differ in cycling benchmarks, so raw byte totals still required manual division before comparing scene-local wire pressure.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: a short actual Xvfb/kitty after-run confirmed debug scene JSON now includes per-scene `upload_bytes_per_frame` and `upload_wire_bytes_per_frame`. Example after-run values included `overview_agents upload_bytes_per_frame=0.0`, `upload_wire_bytes_per_frame=0.0`; `project_beads_board upload_bytes_per_frame≈27,338`, `upload_wire_bytes_per_frame≈36,552`. The after-run had the expected dirty-source caveat because the benchmark binary embeds the prior committed SHA before this local commit.
- Context: this is observability-only. Rendering behavior, cache decisions, upload batching, and terminal output are unchanged.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: extended the benchmark-support scene summary test to cover per-scene upload byte density and wire-byte density.
- Behavioural delta: real-TUI benchmark debug scene JSON now exposes upload byte density per scene; no live TUI rendering behavior changes.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui real_benchmark_result_reports_scene_cache_upload_and_terminal_timing_bd_b613a1_bd_0b861e_bd_76efd8_bd_8ca74c_bd_60c4f1_bd_b31ffa`; `cargo test -p caco-tui app::benchmark_support::tests`; actual `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 3 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`; `cargo clippy -p caco-tui --lib -- -D warnings`; full `cargo test -p caco-tui`; `git diff --check`.

## Operator-takeaway

Scene summaries now normalize both upload counts and upload bytes by local frame count. Future graphics work can compare scene-local byte pressure directly and decide whether a candidate should target output size, upload count, or terminal processing.
