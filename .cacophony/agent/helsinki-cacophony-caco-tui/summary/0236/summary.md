# Session summary — Borrowed project-tree agent row spans

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove several small per-agent-row string allocations from project tree agent rows.

## Bead(s)

- `bd-e05e82` — Borrow project-tree agent row spans.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `a475c9ae7`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈422.5, terminal-inclusive work FPS ≈179.3, avg work ≈2.37ms, avg terminal-inclusive ≈5.58ms, avg upload pass ≈0.72ms. `overview_agents` was ≈898.4 work FPS / avg ≈1.11ms, `project_beads_board` was ≈335.5 / avg ≈2.98ms, and `feed_logs` was ≈413.3 / avg ≈2.42ms.
- Context: project tree agent row rendering formatted indicator+space, type brackets, and display-label prefix into fresh `String`s for each visible agent row.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈492.1, terminal-inclusive work FPS ≈191.2, avg work ≈2.03ms, avg terminal-inclusive ≈5.23ms, avg upload pass ≈0.63ms. `overview_agents` measured ≈827.6 work FPS / avg ≈1.21ms, `project_beads_board` ≈425.4 / avg ≈2.35ms, and `feed_logs` ≈446.7 / avg ≈2.24ms.
- Context: project tree agent rows now split indicator, type brackets, and display-label prefix into borrowed/static spans and move the already-built display label directly. Overview was noisy/lower, but terminal-inclusive and bead/log scenes improved.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/project_tree.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: existing project tree render tests still cover queued/live row placement and chronological ordering.
- Behavioural delta: no intended UI change. The rendered row text stays the same; row construction now uses fewer temporary strings.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo test -p caco-tui render_buckets_mixed_case_queued_agents_as_queued`; `cargo test -p caco-tui render_orders_agents_by_created_at_newest_first_within_node`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

Project tree agent rows now avoid a few repeated formatting allocations per visible row while preserving text and styling; the measured benefit was mixed by scene, so this is best understood as another small hot-path cleanup in the broader TUI allocation-reduction series.
