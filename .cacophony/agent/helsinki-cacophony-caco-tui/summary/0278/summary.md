# Session summary — Cache feed suffix widths

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and reduce repeated width work in the feed row renderer after discarding a separate target-regressing feed experiment.

## Bead(s)

- `bd-6c8d3f` — Cache feed suffix widths.
- `bd-8b40e4` — Accumulate feed row width directly; discarded/demoted to draft after target feed regression.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `eb98bed97`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈475.2, terminal-inclusive work FPS ≈187.3, avg work ≈2.10ms, avg terminal-inclusive ≈5.34ms, avg upload pass ≈0.66ms. `overview_agents` was ≈993.4 work FPS / avg ≈1.01ms, `project_beads_board` was ≈380.8 / avg ≈2.63ms, and `feed_logs` was ≈464.6 / avg ≈2.15ms.
- Context: feed rows repeatedly computed the right suffix width from the same node/project values on consecutive entries. A first experiment to compute left width directly (`bd-8b40e4`) passed tests but regressed target `feed_logs` twice, so it was reverted and the bead was demoted to draft.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈498.4, terminal-inclusive work FPS ≈190.9, avg work ≈2.01ms, avg terminal-inclusive ≈5.24ms, avg upload pass ≈0.62ms. `overview_agents` measured ≈735.5 work FPS / avg ≈1.36ms, `project_beads_board` ≈437.8 / avg ≈2.28ms, and `feed_logs` ≈471.2 / avg ≈2.12ms. The after-run emitted the usual dirty-source warning because the benchmark embeds the last git commit, but it rebuilt without `--no-build`.
- Context: feed rendering now carries a single-entry right-suffix width cache keyed by node/project, preserving the suffix text and padding calculation while avoiding repeated text-width work for consecutive matching rows.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/feed.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: added 1 focused cache test for consecutive feed suffix width reuse.
- Behavioural delta: no intended UI/layout change; right suffix text and padding widths are computed by the same underlying helper, with caching for consecutive identical node/project suffixes.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui views::feed::tests`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

The landed feed change is deliberately smaller than the discarded direct-left-width experiment: it caches only repeated suffix widths and showed a modest target `feed_logs` improvement. The earlier `bd-8b40e4` design is documented as rejected because it regressed the target scene twice.
