# Session summary — Tmux bright-white theme mapping

## Goal

Continue TUI terminal-color polish by fixing captured tmux SGR bright-white handling.

## Bead(s)

- `bd-c9a9fe` — Tmux SGR bright white should map to theme brightest color

## Before state

- Failing tests: none; this was a source-inspection terminal color fidelity task.
- Context: `crates/caco-tui/src/tmux.rs` mapped SGR 97/107 through `Color::Gray`, which resolves to the active theme foreground instead of the bright-white palette slot.

## After state

- Failing tests: none in focused validation.
- Context: SGR 97 and 107 now map to active theme indexed color 15 / brightest foreground semantics, preserving bright-white emphasis in captured tmux output.

## Diff summary

- Commits: `3194807cb`
- Files touched: `crates/caco-tui/src/tmux.rs`
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui parse_ansi_bright_white_uses_theme_brightest_slot --lib`
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui parse_ansi --lib`

## Operator-takeaway

Captured tmux panes now render SGR bright-white foreground/background as the theme’s brightest terminal color instead of normal foreground.
