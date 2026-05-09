# Session summary — Cache feed row theme colors

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove repeated feed-row theme color lookups while preserving rendered feed rows.

## Bead(s)

- `bd-7ef724` — Cache feed row theme colors.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `e500b4878`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈331.5, terminal-inclusive work FPS ≈160.8, avg work ≈3.02ms, avg terminal-inclusive ≈6.22ms, avg upload pass ≈0.86ms. `overview_agents` was ≈699.3 work FPS / avg ≈1.43ms, `project_beads_board` was ≈293.3 / avg ≈3.41ms, and `feed_logs` was ≈298.7 / avg ≈3.35ms.
- Context: feed row rendering called `common::theme()` repeatedly inside each visible row for primary/dim/elevated colors and right-suffix dim styling, even though those colors are stable across the render pass.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈464.7, terminal-inclusive work FPS ≈186.1, avg work ≈2.15ms, avg terminal-inclusive ≈5.37ms, avg upload pass ≈0.64ms. `project_beads_board` measured ≈497.5 work FPS / avg ≈2.01ms, `feed_logs` ≈370.1 / avg ≈2.70ms, and `overview_agents` was lower/noisy at ≈655.1 / avg ≈1.53ms.
- Context: feed rendering now caches `fg_primary`, `fg_dim`, `bg_elevated`, and a dim `Style` once per render and passes the dim style into right-suffix span construction. Dynamic sender/type colors remain per-row.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/feed.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: no new unit test; existing feed row/image/padding tests cover visible behaviour while this is style-preserving refactoring.
- Behavioural delta: no intended UI/layout change; feed row styles are preserved while stable theme color lookups are hoisted out of visible row loops.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo test -p caco-tui feed_padding_spaces_borrows_common_widths_bd_a30423`; `cargo test -p caco-tui feed_entry_with_image_has_fields`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

Feed rows now reuse stable theme-derived colors across the render pass instead of re-reading them for every visible row. The baseline was noisy/low, but actual Kitty evidence improved feed/logs, bead-board, app-side, and terminal-inclusive metrics for this run.
