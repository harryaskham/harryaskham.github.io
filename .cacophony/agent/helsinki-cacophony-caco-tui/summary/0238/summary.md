# Session summary — Preallocated feed and log visible lists

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove small per-frame vector growth reallocations from feed/log visible item rendering.

## Bead(s)

- `bd-2324df` — Preallocate feed and log visible item lists.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `8be1c52e6`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈435.7, terminal-inclusive work FPS ≈181.8, avg work ≈2.30ms, avg terminal-inclusive ≈5.50ms, avg upload pass ≈0.68ms. `overview_agents` was ≈911.3 work FPS / avg ≈1.10ms, `project_beads_board` was ≈351.2 / avg ≈2.85ms, and `feed_logs` was ≈422.1 / avg ≈2.37ms.
- Context: feed/log render paths built visible `ListItem` vectors from empty every frame despite known viewport row budgets, and feed entry line vectors started empty despite predictable image/no-image line counts.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈484.8, terminal-inclusive work FPS ≈190.0, avg work ≈2.06ms, avg terminal-inclusive ≈5.26ms, avg upload pass ≈0.63ms. `overview_agents` measured ≈841.8 work FPS / avg ≈1.19ms, `project_beads_board` ≈396.4 / avg ≈2.52ms, and `feed_logs` ≈460.1 / avg ≈2.17ms.
- Context: feed and log visible item vectors now preallocate to the viewport row budget; feed per-entry line vectors preallocate for either a normal one-line entry or the image placeholder lines.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/feed.rs`, `crates/caco-tui/src/views/logs.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: existing feed padding and log wrapping/visual-height tests cover output after the capacity-only changes.
- Behavioural delta: no intended UI change. Feed/log item contents and wrapping stay the same; vectors allocate with expected capacities.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo test -p caco-tui feed_padding_spaces_borrows_common_widths_bd_a30423`; `cargo test -p caco-tui log_entry_visual_height`; `cargo test -p caco-tui wrap_spans`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

Feed and log rendering now preallocate the obvious visible row vectors instead of growing from empty every frame, improving feed/log benchmark evidence without changing any rendered content.
