# Session summary — Direct bead table graphics panel keys

## Goal

Continue the active caco-tui optimiser loop with real Kitty graphics evidence, discard target-scene regressions, and land one narrow bead-board render-path allocation cleanup that preserves visible and graphics behaviour.

## Bead(s)

- `bd-f05ad8` — Build bead table panel keys directly.
- Discarded during exploration: `bd-6a653c`, `bd-2f4ae3`, `bd-0ff1e4`.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `538efe44e`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈434.0, terminal-inclusive work FPS ≈179.6, avg work ≈2.30ms, avg terminal-inclusive ≈5.57ms, avg upload pass ≈0.69ms. `overview_agents` was ≈930.1 work FPS / avg ≈1.08ms, `project_beads_board` was ≈371.4 / avg ≈2.69ms, and `feed_logs` was ≈397.0 / avg ≈2.52ms.
- Context: the project bead-table render path recorded its graphics panel with `format!("panel:beads:{project}:table")` each frame. Earlier candidate slices in this cycle were rejected when actual Kitty target-scene evidence regressed.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-runs stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). First after-run measured ≈410.4 app FPS / ≈174.8 terminal-inclusive FPS, with `project_beads_board` ≈394.9 work FPS. Immediate rerun measured ≈435.8 app FPS / ≈179.4 terminal-inclusive FPS, avg work ≈2.29ms, avg terminal-inclusive ≈5.57ms, avg upload pass ≈0.67ms, with `project_beads_board` ≈439.4 work FPS / avg ≈2.28ms and `feed_logs` ≈407.4 / avg ≈2.45ms.
- Context: bead-table graphics panel keys now allocate one pre-sized `String` and append the static prefix, project, and suffix directly. Key bytes are preserved.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/beads.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: added a targeted byte-preservation test for `bead_table_panel_key()`.
- Behavioural delta: no intended UI or graphics identity change; only construction of the existing project bead-table graphics panel key changed.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo test -p caco-tui bead_table_panel_key_preserves_format_bytes_bd_f05ad8`; `cargo test -p caco-tui render_filtered_empty_keeps_table_total_bd_004e21`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

This cycle landed a small, byte-preserving bead-board graphics key allocation cleanup and explicitly discarded three plausible but regressing experiments. The useful signal is that `project_beads_board` improved on the rerun, while overall graphics metrics remain host-noisy enough that this should be treated as a narrow allocation cleanup rather than a broad FPS breakthrough.
