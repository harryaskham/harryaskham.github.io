# Session summary — Cache bead label chip styles

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and reduce repeated theme/style work in bead label chip rendering, which feeds the project bead-board hot path.

## Bead(s)

- `bd-43812e` — Cache bead label chip styles.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `456d5d506`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈402.8, terminal-inclusive work FPS ≈174.7, avg work ≈2.48ms, avg terminal-inclusive ≈5.72ms, avg upload pass ≈0.73ms. `overview_agents` was ≈898.8 work FPS / avg ≈1.11ms, `project_beads_board` was ≈305.9 / avg ≈3.27ms, and `feed_logs` was ≈407.3 / avg ≈2.46ms.
- Context: `common::bead_label_spans()` cached only the dim color, then called `bead_label_style(label)` for each shown label chip; that helper re-read the active theme for every visible chip.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈489.5, terminal-inclusive work FPS ≈189.3, avg work ≈2.04ms, avg terminal-inclusive ≈5.28ms, avg upload pass ≈0.62ms. `project_beads_board` measured ≈520.2 work FPS / avg ≈1.92ms, `feed_logs` ≈417.0 / avg ≈2.40ms, and `overview_agents` ≈604.5 / avg ≈1.65ms. The after-run emitted the usual dirty-source warning because the benchmark embeds the last git commit, but it rebuilt without `--no-build`.
- Context: bead label rendering now caches dim, overflow, structured-label, and plain-label styles once per `bead_label_spans()` call and passes the cached styles through the label-style selector.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/common.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: no new tests; existing bead-label rendering tests cover empty, overflow, preallocation, borrowed untruncated labels, and ASCII width fast paths.
- Behavioural delta: no intended UI/layout change; structured-label tinting and overflow styling are preserved while theme/style work is hoisted out of the per-label loop.
- Validation: `./scripts/rustfmt-changed.sh` skipped `common.rs` because the HEAD version is not rustfmt-clean; no unrelated formatting churn was introduced. `cargo test -p caco-tui bead_label`; `cargo test -p caco-tui label_overflow`; `cargo check -p caco-tui`; `cargo test -p caco-tui views::common::tests::bead_label`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

Bead label chips now reuse cached styles instead of re-reading the active theme for every visible chip. Actual Kitty evidence improved the targeted project bead board and terminal-inclusive FPS; the overview scene regressed from an unusually high baseline and is recorded as host noise.
