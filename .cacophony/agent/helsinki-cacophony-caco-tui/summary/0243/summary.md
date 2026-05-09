# Session summary — Preallocate bead table width vectors

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and reduce small vector growth reallocations in bead table rendering without changing layout or visible table content.

## Bead(s)

- `bd-21dc3d` — Preallocate bead table width vectors.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `c9f9ab862`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈335.0, terminal-inclusive work FPS ≈162.0, avg work ≈2.99ms, avg terminal-inclusive ≈6.17ms, avg upload pass ≈0.88ms. `overview_agents` was ≈586.0 work FPS / avg ≈1.71ms, `project_beads_board` was ≈293.5 / avg ≈3.41ms, and `feed_logs` was ≈313.6 / avg ≈3.19ms.
- Context: project/global bead table widths were still constructed from `Vec::new()` every render even though normal and in-progress column counts are known. The existing row-cell preallocation used title-width column constants that were one lower than the actual visible row cell counts.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈444.5, terminal-inclusive work FPS ≈182.7, avg work ≈2.25ms, avg terminal-inclusive ≈5.47ms, avg upload pass ≈0.72ms. `overview_agents` measured ≈755.8 work FPS / avg ≈1.32ms, `project_beads_board` ≈400.3 / avg ≈2.50ms, and `feed_logs` ≈384.1 / avg ≈2.60ms.
- Context: project/global bead table width vectors now preallocate to exact known visible column counts plus optional multiselect checkbox column; row cell vector preallocation uses the same exact counts.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/beads.rs`, `crates/caco-tui/src/views/global_beads.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: existing project/global bead render tests cover populated global rows, multi-project rows, and filtered-empty total handling.
- Behavioural delta: no intended UI change. Table layout constraints and rows render the same; vector capacities are sized up front.
- Validation: `./scripts/rustfmt-changed.sh` formatted `beads.rs` and skipped `global_beads.rs` because the HEAD version is not rustfmt-clean; avoided unrelated formatting churn. Passed `cargo test -p caco-tui render_populated_shows_project_column`; `cargo test -p caco-tui render_shows_beads_from_multiple_projects`; `cargo test -p caco-tui render_filtered_empty_keeps_global_total_bd_004e21`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

The bead table renderer now sizes its small row/width vectors to the actual visible column counts instead of letting them grow during each frame. The baseline was noisy/low, but actual Kitty terminal-inclusive and all scene metrics improved against it.
