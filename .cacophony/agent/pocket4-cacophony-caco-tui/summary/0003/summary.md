# Session summary — tmux passthrough cursor origin fix

## Goal

Fix the TUI graphics placement bug that showed up when kitty graphics were wrapped through tmux passthrough. The goal was to stop graphics from anchoring to the tmux window origin by making cursor-placement commands use the correct outer-terminal coordinates for the current pane.

## Bead(s)

- `bd-537b12` — tmux passthrough mode breaks TUI graphics layout

## Before state

- Failing tests: no explicit regression covered the pane-origin case.
- Relevant metrics: `crates/caco-tui/src/kitty.rs` already wrapped cursor moves in tmux passthrough, but still emitted pane-local `rect.x`/`rect.y` coordinates even though passthrough bypasses tmux's coordinate translation.
- Context: this meant kitty placements could land relative to the tmux window origin instead of the active pane origin, especially in non-zero-offset panes.

## After state

- Failing tests: none in the targeted kitty test run.
- Relevant metrics: tmux passthrough cursor moves now add the current pane's `pane_left`/`pane_top` origin, refreshed at startup, on terminal resize, and once per graphics upload pass when passthrough is active.
- Context: targeted `cargo test -p caco-tui cursor_move_command_tmux_passthrough -- --nocapture` passed with two tmux-origin-focused tests.

## Diff summary

- Commits: `bf2d53a34`
- Files touched: `crates/caco-tui/src/kitty.rs`, `crates/caco-tui/src/app.rs`
- Tests: +1 / -0 / flipped 0
- Behavioural delta: kitty graphics cursor moves in tmux passthrough now use outer-terminal coordinates derived from the current pane origin, so placements align with the real pane instead of drifting toward tmux window `0,0`.

## Operator-takeaway

The bug was not in the kitty placement command itself; it was in the coordinate space used before the placement. Once tmux passthrough is active, cursor moves must be translated from pane-local coordinates to tmux-window coordinates, and this session adds that translation plus a direct regression test for it.