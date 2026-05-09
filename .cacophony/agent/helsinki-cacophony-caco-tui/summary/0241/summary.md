# Session summary — Borrow global bead row labels

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove small per-visible-row allocations from global bead table rendering by reusing the borrowed helpers already used by project bead rows.

## Bead(s)

- `bd-7f590a` — Borrow global bead row labels.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `ecd5e21e4`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈358.9, terminal-inclusive work FPS ≈166.4, avg work ≈2.79ms, avg terminal-inclusive ≈6.01ms, avg upload pass ≈0.83ms. `overview_agents` was ≈642.6 work FPS / avg ≈1.56ms, `project_beads_board` was ≈299.2 / avg ≈3.34ms, and `feed_logs` was ≈359.5 / avg ≈2.78ms.
- Context: global bead rows still allocated priority label strings with `priority_label(...).to_string()` and title strings with `common::truncate()`, while project bead rows had already moved to borrowed priority labels and `truncate_cow()`.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈590.7, terminal-inclusive work FPS ≈203.3, avg work ≈1.69ms, avg terminal-inclusive ≈4.92ms, avg upload pass ≈0.59ms. `overview_agents` measured ≈993.4 work FPS / avg ≈1.01ms, `project_beads_board` ≈552.9 / avg ≈1.81ms, and `feed_logs` ≈499.2 / avg ≈2.00ms.
- Context: global bead rows now borrow static priority labels and use `common::truncate_cow()` for titles, so untruncated titles borrow and only genuinely truncated titles allocate.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/global_beads.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: existing global bead render tests cover project column display, multi-project rendering, and title truncation output.
- Behavioural delta: no intended UI change. Global bead priority/title cells render the same text with fewer common-row allocations.
- Validation: `./scripts/rustfmt-changed.sh` was attempted but skipped `global_beads.rs` because the HEAD version is not rustfmt-clean; avoided unrelated formatting churn. Passed `cargo test -p caco-tui render_populated_shows_project_column`; `cargo test -p caco-tui render_uses_available_global_title_width_before_truncating`; `cargo test -p caco-tui render_shows_beads_from_multiple_projects`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

Global bead rows now use the same borrowed label/title pattern as project bead rows, eliminating avoidable string allocation on common visible rows. The measured baseline was noisy and low, but the actual Kitty after-run improved all reported scenes and terminal-inclusive FPS against it.
