# Session summary — Kitty graphics passthrough from TUI-owned PTYs

## Goal

Start implementing the requested support for kitty graphics emitted inside TUI-owned PTYs / nested tmux sessions so agent plugins that draw graphics can be used from embedded terminal panes.

## Bead(s)

- `bd-3990c8` — TUI-owned PTYs should pass through kitty graphics from nested tmux

## Before state

- Failing tests: none for this path; there was no focused coverage for PTY kitty graphics extraction.
- Relevant metrics: not benchmarked; this is an interaction/terminal capability slice.
- Context: owned PTY output was fed only into the vt100 parser. Kitty APC payloads (including tmux passthrough DCS wrappers) were consumed as terminal control bytes and never reached the outer graphics-capable terminal.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: the PTY reader now extracts complete raw kitty APC payloads and tmux passthrough wrappers containing kitty graphics, including payloads split across PTY read boundaries. It emits a TUI action event, and the app forwards those payloads to stdout when the outer TUI is kitty-capable. Raw APC payloads are wrapped in tmux passthrough when the TUI itself is inside tmux.

## Diff summary

- Commits: `6f6b1432b`, `e65dda561`
- Files touched: `crates/caco-tui/src/pty.rs`, `crates/caco-tui/src/event.rs`, `crates/caco-tui/src/app.rs`, `crates/caco-tui/src/kitty.rs`
- Tests: +5 focused PTY kitty payload extraction tests / existing tmux passthrough tests preserved
- Behavioural delta: TUI-owned PTY output can now forward nested kitty graphics payloads to the outer terminal instead of dropping them into the vt100-only render path.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui extract_kitty_graphics_payloads --lib`
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui kitty_graphics_extractor --lib`
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui tmux_passthrough --lib`

## Operator-takeaway

This is the first practical passthrough slice for graphics in embedded TUI PTYs: complete kitty graphics payloads are detected and forwarded, including nested tmux passthrough wrappers.
