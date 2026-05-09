# Session summary — Avoid project overview agent Vec

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove a small allocation/scanning hot path from the project overview half of the `overview_agents` benchmark scene.

## Bead(s)

- `bd-bb2d61` — Avoid project overview agent Vec.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `222a8e198`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈444.2, terminal-inclusive work FPS ≈181.8, avg work ≈2.25ms, avg terminal-inclusive ≈5.50ms, avg upload pass ≈0.68ms. `overview_agents` was ≈730.3 work FPS / avg ≈1.37ms, `project_beads_board` was ≈358.8 / avg ≈2.79ms, and `feed_logs` was ≈467.4 / avg ≈2.14ms.
- Context: `render_project_info()` allocated a `Vec` of all agents in the project, then scanned it again for running count and node set.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-runs stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235). First after-run improved headline metrics but lowered target `overview_agents` and `feed_logs`. Rerun: app-side work FPS ≈497.4, terminal-inclusive work FPS ≈189.2, avg work ≈2.01ms, avg terminal-inclusive ≈5.28ms. `overview_agents` measured ≈812.2 work FPS / avg ≈1.23ms, `project_beads_board` ≈480.6 / avg ≈2.08ms, and `feed_logs` ≈409.8 / avg ≈2.44ms. After-runs emitted the usual dirty-source warning because the benchmark embeds the last git commit, but rebuilt without `--no-build`.
- Context: project info now counts total/running agents and fills the machine set in one pass over matching agents, preserving visible counts and node totals without allocating the intermediate project-agent vector.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/project_overview.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: no new tests; existing project overview count tests plus full caco-tui tests passed.
- Behavioural delta: no intended UI/layout change; project overview still reports the same agent counts and node total.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui project_overview`; `cargo test -p caco-tui project_agents`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

Project overview now avoids an intermediate project-agent vector in the overview scene. The after rerun improved the target overview and headline metrics, while the unrelated feed/log scene regressed, so this is landed as a scoped overview allocation cleanup with mixed scene evidence.
