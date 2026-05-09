# Session summary — Cache bead row theme colors

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence, discard one regressing project-row metadata experiment, and land a narrow bead-row render-path cleanup that avoids repeated static theme color lookups.

## Bead(s)

- `bd-a3088c` — Cache bead row theme colors.
- Discarded during exploration: `bd-c5423b` — Reuse prepared bead row metadata.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `17cacd8e1`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈466.3, terminal-inclusive work FPS ≈183.4, avg work ≈2.14ms, avg terminal-inclusive ≈5.45ms, avg upload pass ≈0.66ms. `overview_agents` was ≈786.7 work FPS / avg ≈1.27ms, `project_beads_board` was ≈397.7 / avg ≈2.51ms, and `feed_logs` was ≈455.8 / avg ≈2.19ms.
- Context: dense project/global bead rows still called `common::theme()` repeatedly for static row colors such as ID/title/type/assignee/dependency/update/checkbox/mark colors. A separate experiment (`bd-c5423b`) carrying prepared sort metadata into rows passed focused tests but regressed the target scene and was reverted.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-runs stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). First after-run was lower/noisy at ≈426.8 app FPS / ≈180.3 terminal-inclusive FPS, with `project_beads_board` ≈375.3. Immediate rerun improved to ≈583.4 app FPS / ≈202.3 terminal-inclusive FPS, avg work ≈1.71ms, avg terminal-inclusive ≈4.94ms, avg upload pass ≈0.57ms. Rerun scene metrics: `overview_agents` ≈980.1 work FPS / avg ≈1.02ms, `project_beads_board` ≈535.7 / avg ≈1.87ms, `feed_logs` ≈503.4 / avg ≈1.99ms.
- Context: project and global bead table render paths now cache the active theme reference plus repeated row colors once per table render. Per-row dynamic priority/status colors remain computed by their helpers.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/beads.rs`, `crates/caco-tui/src/views/global_beads.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: no new unit test; existing project/global bead render tests cover visible row output while the change is style-preserving refactoring.
- Behavioural delta: no intended UI/layout change; styles are preserved while repeated theme accessors are hoisted out of each visible row.
- Validation: `./scripts/rustfmt-changed.sh` attempted and formatted `beads.rs`; it skipped pre-existing non-rustfmt-clean `global_beads.rs` to avoid unrelated formatting churn. Passed `cargo test -p caco-tui render_filtered_empty_keeps_table_total_bd_004e21`; `cargo test -p caco-tui render_filtered_empty_keeps_global_total_bd_004e21`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

The landed change is a small style-preserving hot-path cleanup: bead table rows now reuse per-render theme colors instead of asking the theme repeatedly per visible row. The first graphics run was noisy, but the immediate rerun showed strong improvements in the target bead-board scene and overall terminal-inclusive headroom.
