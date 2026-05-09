# Session summary — Fast path unfiltered project bead sort

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and reduce common unfiltered project bead-board sorting overhead.

## Bead(s)

- `bd-7fc2c4` — Fast path unfiltered project bead sort.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `34ad89bf2`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈437.8, terminal-inclusive work FPS ≈180.3, avg work ≈2.28ms, avg terminal-inclusive ≈5.55ms, avg upload pass ≈0.69ms. `overview_agents` was ≈808.0 work FPS / avg ≈1.24ms, `project_beads_board` was ≈368.2 / avg ≈2.72ms, and `feed_logs` was ≈424.3 / avg ≈2.36ms.
- Context: `sorted_beads_for_project_section()` called the generic `bead_matches_filters()` helper for every project bead even when there was no active search, filter, or section state.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈480.0, terminal-inclusive work FPS ≈186.6, avg work ≈2.08ms, avg terminal-inclusive ≈5.36ms, avg upload pass ≈0.67ms. `overview_agents` measured ≈899.1 work FPS / avg ≈1.11ms, `project_beads_board` ≈521.4 / avg ≈1.92ms, and `feed_logs` ≈335.6 / avg ≈2.98ms. The after-run emitted the usual dirty-source warning because the benchmark embeds the last git commit, but it rebuilt without `--no-build`.
- Context: the unfiltered project bead list now fast-paths to project-only filtering before sorting, preserving the generic helper for active search/filter/section cases.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/state/mod.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: no new tests; existing project bead filter/render tests plus full caco-tui tests passed.
- Behavioural delta: no intended UI/layout change; unfiltered project bead rows are sorted through the same sorter, while filtered and sectioned paths keep existing behavior.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui sorted_beads_for_project`; `cargo test -p caco-tui views::beads::tests`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

The common project bead-board render path now skips unnecessary generic filter checks. Actual Kitty evidence improved the target bead-board, overview, app-side, and terminal-inclusive metrics, while the unrelated feed/log scene regressed and was recorded as out-of-path noise.
