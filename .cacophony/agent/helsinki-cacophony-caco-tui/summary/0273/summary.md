# Session summary — Use bead stats for overview count

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove an avoidable per-frame project-bead map scan from the project overview counts/actions row.

## Bead(s)

- `bd-7e0f23` — Use bead stats for overview count.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `244aefcf1`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈351.4, terminal-inclusive work FPS ≈162.2, avg work ≈2.85ms, avg terminal-inclusive ≈6.16ms, avg upload pass ≈0.75ms. `overview_agents` was ≈501.7 work FPS / avg ≈1.99ms, `project_beads_board` was ≈317.0 / avg ≈3.15ms, and `feed_logs` was ≈341.8 / avg ≈2.93ms.
- Context: `render_counts_and_actions()` scanned `state.beads` for the project bead total on every overview render even though daemon snapshots already provide authoritative per-project `bead_stats.total`.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈437.3, terminal-inclusive work FPS ≈179.5, avg work ≈2.29ms, avg terminal-inclusive ≈5.57ms, avg upload pass ≈0.66ms. `overview_agents` measured ≈638.1 work FPS / avg ≈1.57ms, `project_beads_board` ≈412.1 / avg ≈2.43ms, and `feed_logs` ≈387.5 / avg ≈2.58ms. The after-run emitted the usual dirty-source warning because the benchmark embeds the last git commit, but it rebuilt without `--no-build`.
- Context: overview counts now prefer authoritative `bead_stats.total` and keep the old loaded-beads scan as a compatibility fallback for missing stats.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/project_overview.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: added 2 focused helper tests for stats preference and loaded-beads fallback.
- Behavioural delta: no intended visual/layout change when snapshot stats are present; the project overview count now matches the authoritative stats source already used by the Beads health section, with the previous loaded-bead count retained only when stats are absent.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui project_bead_total_for_overview`; `cargo test -p caco-tui project_overview`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

The project overview no longer walks the loaded bead map just to render its top-level bead count when authoritative project stats are already available. The actual Kitty benchmark improved all measured scenes in this noisy run, and the fallback keeps compatibility for snapshots without stats.
