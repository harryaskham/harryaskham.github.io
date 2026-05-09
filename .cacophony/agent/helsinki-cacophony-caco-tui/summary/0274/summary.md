# Session summary — Stream overview recent activity rows

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove temporary row-collection allocations from the project overview recent activity renderer.

## Bead(s)

- `bd-42a4cd` — Stream overview recent activity rows.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `daa260c19`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈388.2, terminal-inclusive work FPS ≈170.7, avg work ≈2.58ms, avg terminal-inclusive ≈5.86ms, avg upload pass ≈0.75ms. `overview_agents` was ≈743.6 work FPS / avg ≈1.34ms, `project_beads_board` was ≈298.2 / avg ≈3.35ms, and `feed_logs` was ≈409.4 / avg ≈2.44ms.
- Context: `render_activity()` collected temporary vectors for matching project spawn attempts and recent project feed entries before rendering only small visible subsets.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: first actual Xvfb/kitty after-run improved headline, bead-board, and feed metrics but lowered the target `overview_agents` scene, so I reran before keeping it. The rerun stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB) and measured app-side work FPS ≈523.9, terminal-inclusive work FPS ≈193.6, avg work ≈1.91ms, avg terminal-inclusive ≈5.16ms, avg upload pass ≈0.64ms. `overview_agents` measured ≈928.2 work FPS / avg ≈1.08ms, `project_beads_board` ≈493.3 / avg ≈2.03ms, and `feed_logs` ≈430.0 / avg ≈2.33ms. After-runs emitted the usual dirty-source warning because the benchmark embeds the last git commit, but they rebuilt without `--no-build`.
- Context: recent activity now streams matching spawn attempts/feed entries directly into the visible item vectors, using a count/peek for layout and empty-state decisions.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/project_overview.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: added 2 focused render tests for matching project-feed rows and empty-state preservation.
- Behavioural delta: no intended UI/layout change; project overview recent activity still renders only matching project feed rows and preserves the empty-state copy, but avoids intermediate match vectors.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui render_activity_stream`; `cargo test -p caco-tui project_overview`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'` with a rerun because the first after-run had mixed target evidence. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

Project overview recent activity rendering now avoids temporary match vectors while preserving output. Because the first after-run regressed the target scene, I reran actual Kitty evidence and only kept the slice after the rerun improved the target overview scene and overall terminal-inclusive FPS.
