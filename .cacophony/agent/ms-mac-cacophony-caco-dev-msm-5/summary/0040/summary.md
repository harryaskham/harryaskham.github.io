# Session summary — TUI summaries detail scroll keys

## Goal

Continue TUI summaries usability polish by letting keyboard users scroll the selected summary detail without changing the list selection.

## Bead(s)

- `bd-d03247` — TUI summaries: add keyboard detail scrolling
- related: `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android

## Before state

- TUI summaries list navigation worked with arrows, vim keys, paging, and boundary keys.
- Detail content could be scrolled by mouse/trackpad, but keyboard scrolling was not directly advertised or handled in the summaries view.
- Long summaries required pointer/terminal scroll support to inspect lower sections while keeping the selected row stable.

## After state

- `Ctrl-U` scrolls the selected summary detail upward by a half-page-ish step.
- `Ctrl-D` scrolls the selected summary detail downward and clamps against the existing detail line estimate.
- The footer help now advertises `Ctrl-U/D detail scroll`.
- List selection remains unchanged when detail scrolling.

## Diff summary

- Commits: current `bd-d03247` implementation commit
- Files touched:
  - `crates/caco-tui/src/app.rs`
  - `crates/caco-tui/src/views/summaries.rs`
- Tests:
  - `cargo test -p caco-tui summaries --lib` — passed
  - `cargo test-small` — 256 passed
- Behavioural delta: keyboard-only TUI users can scroll summary detail content independently of row selection.

## Operator-takeaway

The TUI summaries viewer is now more usable for long recorded summaries because detail-pane scrolling is available from the keyboard and shown in the help line.
