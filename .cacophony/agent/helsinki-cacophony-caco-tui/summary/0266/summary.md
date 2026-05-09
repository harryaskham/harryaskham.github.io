# Session summary — Borrow agent list indicator cells

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove a narrow per-row allocation in agent list rendering without changing the rendered agent-list text or layout.

## Bead(s)

- `bd-dc3dfb` — Borrow agent list indicator spans.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `120e4ca90`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈377.1, terminal-inclusive work FPS ≈168.7, avg work ≈2.65ms, avg terminal-inclusive ≈5.93ms, avg upload pass ≈0.75ms. `overview_agents` was ≈706.1 work FPS / avg ≈1.42ms, `project_beads_board` was ≈277.1 / avg ≈3.61ms, and `feed_logs` was ≈410.9 / avg ≈2.43ms.
- Context: after `bd-beac15`, per-node and aggregate agent rows still allocated short `String`s for `format!("{indicator} ")` and padded state-icon formatting. A first attempt that split the cells into extra borrowed spans regressed target `overview_agents` on two actual Kitty runs, so it was reverted before landing.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈488.2, terminal-inclusive work FPS ≈187.9, avg work ≈2.05ms, avg terminal-inclusive ≈5.32ms, avg upload pass ≈0.66ms. `overview_agents` measured ≈757.6 work FPS / avg ≈1.32ms, `project_beads_board` ≈474.4 / avg ≈2.11ms, and `feed_logs` ≈409.2 / avg ≈2.44ms. The after-run emitted the usual dirty-source warning because the benchmark embeds the last git commit, but it rebuilt without `--no-build`.
- Context: `common::agent_indicator_with_space()` returns the exact static indicator-plus-space strings used by dense agent rows, keeping one span per indicator cell while removing short `format!` allocations.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/app.rs`, `crates/caco-tui/src/views/common.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: +1 compatibility test for `agent_indicator_with_space()` matching `agent_indicator()` plus a trailing space.
- Behavioural delta: no intended UI/layout change; dense agent lists render the same indicator text, colors, and selected-row overrides while avoiding two short per-row string formats.
- Validation: `./scripts/rustfmt-changed.sh` formatted `app.rs` and skipped pre-existing non-rustfmt-clean `common.rs`; `cargo check -p caco-tui`; `cargo test -p caco-tui agent_indicator_with_space_matches_indicator_bd_dc3dfb`; `cargo test -p caco-tui project_agents`; `cargo test -p caco-tui status_group`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

The first allocation-removal shape for this hot path was correctly rejected because it regressed the target scene; the landed shape keeps the same one-span cell layout and shows positive actual Kitty evidence for `overview_agents`, app-side FPS, and terminal-inclusive FPS.
