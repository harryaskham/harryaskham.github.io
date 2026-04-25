# Session summary — TUI summaries detail scroll position

## Goal

Continue TUI summaries long-detail usability polish by making detail-pane scroll state visible when a recorded summary is longer than the viewport.

## Bead(s)

- `bd-e51e62` — TUI summaries: show detail scroll position
- related: `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android

## Before state

- TUI summaries supported independent detail scrolling with `Ctrl-U` and `Ctrl-D`.
- A scrollbar appeared for overflowing detail content, but the text help always used a generic tip.
- Keyboard users could scroll long details but did not get an explicit numeric sense of progress through the detail body.

## After state

- Overflowing detail panes now show a footer hint like `Detail scroll X/Y`.
- The same footer advertises `Ctrl-U/D scroll detail` and clarifies that `j/k` still moves the list.
- Non-overflowing detail panes keep a shorter generic tip.

## Diff summary

- Commits: current `bd-e51e62` implementation commit
- Files touched:
  - `crates/caco-tui/src/views/summaries.rs`
- Tests:
  - `cargo test -p caco-tui summaries --lib` — passed
  - `cargo test-small` — 256 passed
- Behavioural delta: long TUI summary details now communicate scroll position and available detail-scroll controls directly in the pane.

## Operator-takeaway

The TUI summaries detail pane now gives keyboard users a clearer sense of where they are inside long recorded summaries, making the detail-scroll controls feel intentional rather than hidden.
