# Session summary — ASCII-fast feed row width accounting

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove remaining ASCII-heavy character-count scans from feed row right-alignment width accounting.

## Bead(s)

- `bd-0b28ba` — Fast-path ASCII feed row width accounting.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `8d28d8370`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈401.2, terminal-inclusive work FPS ≈174.1, avg work ≈2.49ms, avg terminal-inclusive ≈5.74ms, avg upload pass ≈0.71ms. `project_beads_board` was ≈345.5 work FPS / avg ≈2.89ms; `feed_logs` was ≈373.1 work FPS / avg ≈2.68ms.
- Context: feed row rendering still used `.chars().count()` to sum left-side span widths and right node/project suffix widths, even though `common::text_width()` now has an ASCII fast path and Unicode fallback.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈474.9, terminal-inclusive work FPS ≈186.1, avg work ≈2.11ms, avg terminal-inclusive ≈5.37ms, avg upload pass ≈0.63ms. `project_beads_board` measured ≈439.4 work FPS / avg ≈2.28ms and `feed_logs` measured ≈450.8 work FPS / avg ≈2.22ms.
- Context: feed row left-span width sums and right node/project suffix width calculations now use `common::text_width()`, preserving Unicode fallback and visual right alignment.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/feed.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: extended the existing feed helper test with a Unicode node/project suffix case so the width helper fallback stays covered.
- Behavioural delta: no intended UI change. Feed row padding/right-alignment widths remain character-count based; ASCII values now avoid Unicode char iteration.
- Validation: `cargo test -p caco-tui borrowed_feed_helpers`; `cargo test -p caco-tui feed_padding_spaces_borrows_common_widths_bd_a30423`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

Feed row alignment now shares the same ASCII-fast text-width helper used by bead tables, removing another repeated ASCII hot-path char scan while keeping Unicode-safe alignment and visible output unchanged.
