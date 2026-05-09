# Session summary — Use fixed bead table row cells

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove another per-row allocation from the common bead board render path.

## Bead(s)

- `bd-495695` — Use fixed bead table row cells.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings. Another agent reported owning the unrelated `cargo clippy -p caco-tui --all-targets` logs.rs failures under `bd-7a3525`; this slice intentionally did not touch that work.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `9922977b6`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈448.0, terminal-inclusive work FPS ≈182.6, avg work ≈2.23ms, avg terminal-inclusive ≈5.48ms, avg upload pass ≈0.72ms. `overview_agents` was ≈948.5 work FPS / avg ≈1.05ms, `project_beads_board` was ≈360.1 / avg ≈2.78ms, and `feed_logs` was ≈434.0 / avg ≈2.30ms.
- Context: after `bd-4120d3` removed the fixed-width `Vec<Constraint>` in normal bead tables, each visible non-multiselect bead row still allocated a heap `Vec<Line>` even though its column set is fixed.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈575.9, terminal-inclusive work FPS ≈200.7, avg work ≈1.74ms, avg terminal-inclusive ≈4.98ms, avg upload pass ≈0.58ms. `overview_agents` measured ≈933.8 work FPS / avg ≈1.07ms, `project_beads_board` ≈549.0 / avg ≈1.82ms, and `feed_logs` ≈484.4 / avg ≈2.06ms. The after-run emitted the usual dirty-source warning because the benchmark embeds the last git commit, but it rebuilt without `--no-build`.
- Context: non-multiselect normal and in-progress bead rows now pass fixed `Line` arrays to `Row::new()`, while multiselect continues to use `Vec<Line>` because the checkbox column is conditional.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/beads.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: no new tests; existing bead render tests cover populated, empty, narrow, error, reconnecting, and title-width behavior.
- Behavioural delta: no intended UI/layout change; normal/in-progress bead rows keep the same column order and node-column behavior, while multiselect behavior stays on the existing vector path.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui views::beads::tests`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4` / `bd-7a3525`.

## Operator-takeaway

The bead board's common row-render path now avoids heap row-cell vectors as well as fixed-width vectors. Actual Kitty evidence showed a large target `project_beads_board` gain and better terminal-inclusive FPS, with only a slight out-of-path overview dip from a high baseline.
