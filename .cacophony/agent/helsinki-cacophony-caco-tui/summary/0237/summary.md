# Session summary — Preallocated bead table row cells

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove small per-row vector growth reallocations from dense bead table rendering.

## Bead(s)

- `bd-012d76` — Preallocate bead table row cells.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `dc686f8da`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈417.5, terminal-inclusive work FPS ≈178.2, avg work ≈2.40ms, avg terminal-inclusive ≈5.61ms, avg upload pass ≈0.72ms. `overview_agents` was ≈768.8 work FPS / avg ≈1.30ms, `project_beads_board` was ≈365.7 / avg ≈2.73ms, and `feed_logs` was ≈391.9 / avg ≈2.55ms.
- Context: project/global bead table row rendering created `Vec::new()` for every visible row, then pushed 8–10 cells, causing small growth reallocations on a dense hot path.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈495.6, terminal-inclusive work FPS ≈190.9, avg work ≈2.02ms, avg terminal-inclusive ≈5.24ms, avg upload pass ≈0.62ms. `overview_agents` measured ≈818.7 work FPS / avg ≈1.22ms, `project_beads_board` ≈513.0 / avg ≈1.95ms, and `feed_logs` ≈381.6 / avg ≈2.62ms.
- Context: project/global bead table row cell vectors now preallocate to the known column count plus optional multiselect column. Feed/logs was lower/noisy while bead-board and overall metrics improved.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/beads.rs`, `crates/caco-tui/src/views/global_beads.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: existing project/global bead render tests cover visible table output after the capacity-only change.
- Behavioural delta: no intended UI change. Row cells are the same; vectors allocate once at the expected capacity.
- Validation: `./scripts/rustfmt-changed.sh` formatted `beads.rs` and skipped pre-existing non-rustfmt-clean `global_beads.rs` to avoid unrelated churn; `cargo test -p caco-tui render_filtered_empty_keeps_global_total_bd_004e21`; `cargo test -p caco-tui render_populated_shows_project_column`; `cargo test -p caco-tui render_shows_beads_from_multiple_projects`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

Dense bead table rows now preallocate their expected cell count instead of growing from an empty vector, removing another small allocation hot path while keeping the table output unchanged.
