# Session summary — Preallocate bead table header cells

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove one more small per-frame vector growth allocation from bead table header rendering.

## Bead(s)

- `bd-469c75` — Preallocate bead table header cells.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `3c785766b`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈401.1, terminal-inclusive work FPS ≈175.2, avg work ≈2.49ms, avg terminal-inclusive ≈5.71ms, avg upload pass ≈0.78ms. `overview_agents` was ≈776.4 work FPS / avg ≈1.29ms, `project_beads_board` was ≈332.8 / avg ≈3.01ms, and `feed_logs` was ≈395.9 / avg ≈2.53ms.
- Context: project/global bead table `header_cells` still started from `Vec::new()` every render, despite known active header column counts and a single optional multiselect checkbox column.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈450.9, terminal-inclusive work FPS ≈184.7, avg work ≈2.22ms, avg terminal-inclusive ≈5.42ms, avg upload pass ≈0.69ms. `overview_agents` measured ≈631.2 work FPS / avg ≈1.58ms, `project_beads_board` ≈464.9 / avg ≈2.15ms, and `feed_logs` ≈372.5 / avg ≈2.68ms.
- Context: project/global bead table header vectors now preallocate to `active_columns.len()` plus the optional multiselect checkbox column. This is a targeted bead-board cleanup; overview/feed deltas were mixed/noisy.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/beads.rs`, `crates/caco-tui/src/views/global_beads.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: existing project/global bead render tests cover populated global rows, multi-project rows, and filtered-empty total handling.
- Behavioural delta: no intended UI change. Bead table headers render the same cells; the vectors are simply sized up front.
- Validation: `./scripts/rustfmt-changed.sh` formatted `beads.rs` and skipped `global_beads.rs` because the HEAD version is not rustfmt-clean; avoided unrelated formatting churn. Passed `cargo test -p caco-tui render_populated_shows_project_column`; `cargo test -p caco-tui render_shows_beads_from_multiple_projects`; `cargo test -p caco-tui render_filtered_empty_keeps_global_total_bd_004e21`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

Bead table headers now preallocate the small header-cell vector to the known column count. The measured win is targeted at bead-board frames: project_beads_board and overall terminal-inclusive metrics improved, while overview/feed scenes remained noisy and mixed.
