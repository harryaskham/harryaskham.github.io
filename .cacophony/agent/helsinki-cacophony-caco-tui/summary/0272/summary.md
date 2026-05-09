# Session summary — Cache project overview info styles

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and reduce repeated theme/style construction in the project overview half of the `overview_agents` benchmark scene.

## Bead(s)

- `bd-1e94cf` — Cache project overview info styles.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `debe59eaf`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈423.3, terminal-inclusive work FPS ≈178.3, avg work ≈2.36ms, avg terminal-inclusive ≈5.61ms, avg upload pass ≈0.72ms. `overview_agents` was ≈729.3 work FPS / avg ≈1.37ms, `project_beads_board` was ≈346.4 / avg ≈2.89ms, and `feed_logs` was ≈427.2 / avg ≈2.34ms.
- Context: after the project-agent vector fix, `render_project_info()` still repeatedly called `common::theme()` and rebuilt stable `Style::default().fg(...)` values for project, remote, branch, count, and mode rows.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈577.2, terminal-inclusive work FPS ≈200.3, avg work ≈1.73ms, avg terminal-inclusive ≈4.99ms, avg upload pass ≈0.58ms. `overview_agents` measured ≈944.8 work FPS / avg ≈1.06ms, `project_beads_board` ≈522.1 / avg ≈1.92ms, and `feed_logs` ≈510.1 / avg ≈1.96ms. The after-run emitted the usual dirty-source warning because the benchmark embeds the last git commit, but it rebuilt without `--no-build`.
- Context: project info now caches the small set of theme-derived styles once per render and reuses them across the project info rows while preserving dynamic mode color selection.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/project_overview.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: no new tests; existing project overview tests plus full caco-tui tests passed.
- Behavioural delta: no intended UI/layout change; project info text and colors are preserved, but stable styles are built once per render instead of repeatedly.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui project_overview`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

The overview project-info block now reuses stable theme styles. Actual Kitty evidence improved all benchmark scenes plus both app-side and terminal-inclusive FPS in this run.
