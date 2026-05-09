# Session summary — Preallocate feed row suffix spans

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove a per-visible-row feed allocation growth caused by appending right-side suffix spans after constructing the fixed base span list.

## Bead(s)

- `bd-70109d` — Preallocate feed row suffix spans.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `c0b4a8fbe`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈377.6, terminal-inclusive work FPS ≈171.3, avg work ≈2.65ms, avg terminal-inclusive ≈5.84ms, avg upload pass ≈0.82ms. `overview_agents` was ≈619.2 work FPS / avg ≈1.61ms, `project_beads_board` was ≈304.5 / avg ≈3.28ms, and `feed_logs` was ≈404.2 / avg ≈2.47ms.
- Context: feed rows built seven base spans with `vec![...]`, then common rows with a right suffix pushed padding plus one to four suffix spans, forcing vector growth beyond capacity seven.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈513.8, terminal-inclusive work FPS ≈193.4, avg work ≈1.95ms, avg terminal-inclusive ≈5.17ms, avg upload pass ≈0.65ms. `overview_agents` measured ≈859.6 work FPS / avg ≈1.16ms, `project_beads_board` ≈500.9 / avg ≈2.00ms, and `feed_logs` ≈416.8 / avg ≈2.40ms.
- Context: feed row spans now preallocate to the seven base spans plus a worst-case padding/right-suffix span budget when a right suffix is present. Rendered feed rows are unchanged.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/feed.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: +1 feed helper test for suffix span capacity; existing feed helper/padding tests preserve row text and suffix behaviour.
- Behavioural delta: no intended UI change. Feed rows render the same spans; their vector capacity is sized before suffix appends.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo test -p caco-tui feed_row_span_capacity_reserves_suffix_growth_bd_70109d`; `cargo test -p caco-tui borrowed_feed_helpers`; `cargo test -p caco-tui feed_padding_spaces_borrows_common_widths_bd_a30423`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

Feed rows now reserve enough span capacity for the common right-suffix path instead of growing from the seven-span base allocation. This is a small allocation cleanup, with actual Kitty evidence improving terminal-inclusive and all scene metrics against the fresh baseline.
