# Session summary — Use fixed bead table widths

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove a per-frame width-vector allocation from the common bead table render path.

## Bead(s)

- `bd-4120d3` — Use fixed bead table widths.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `b316a42ef`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈443.8, terminal-inclusive work FPS ≈181.5, avg work ≈2.25ms, avg terminal-inclusive ≈5.51ms, avg upload pass ≈0.67ms. `overview_agents` was ≈977.0 work FPS / avg ≈1.02ms, `project_beads_board` was ≈369.4 / avg ≈2.71ms, and `feed_logs` was ≈414.0 / avg ≈2.42ms.
- Context: `render_table()` allocated a `Vec<Constraint>` for column widths every frame, even when multiselect was off and the normal/in-progress bead table column sets were fixed.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈542.7, terminal-inclusive work FPS ≈196.9, avg work ≈1.84ms, avg terminal-inclusive ≈5.08ms, avg upload pass ≈0.59ms. `overview_agents` measured ≈950.4 work FPS / avg ≈1.05ms, `project_beads_board` ≈535.5 / avg ≈1.87ms, and `feed_logs` ≈427.7 / avg ≈2.34ms. The after-run emitted the usual dirty-source warning because the benchmark embeds the last git commit, but it rebuilt without `--no-build`.
- Context: non-multiselect bead tables now pass fixed `Constraint` arrays to `Table::new()`, and the heap width vector remains only for multiselect's extra checkbox column.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/beads.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: no new tests; existing bead render tests cover the preserved populated/empty/error/narrow table behavior.
- Behavioural delta: no intended UI/layout change; normal and in-progress bead tables keep the same constraints, while multiselect still builds a vector to prepend the checkbox column.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui views::beads::tests`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

The bead board's common render path no longer allocates a heap widths vector every frame. Actual Kitty evidence showed a strong target `project_beads_board` improvement, with a small out-of-path overview dip from a high baseline.
