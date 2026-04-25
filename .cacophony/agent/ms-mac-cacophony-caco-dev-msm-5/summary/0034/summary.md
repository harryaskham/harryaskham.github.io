# Session summary — TUI summaries Ctrl-U filter clear

## Goal

Continue TUI summaries keyboard polish by making the slash filter easier to edit during long-history searches.

## Bead(s)

- `bd-c9b855` — TUI summaries: support clearing filter with Ctrl-U
- related: `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android

## Before state

- The TUI summaries slash filter supported typed characters, Backspace, Enter, and Escape.
- Clearing a long query required repeated Backspace or exiting the filter state.
- Selection and detail scroll already reset when the query changed.

## After state

- While the summaries filter is active, `Ctrl-U` clears the entire query in one keystroke.
- Selection resets to the top of the filtered/unfiltered list after clearing.
- Detail scroll resets to the top to keep list/detail context aligned.

## Diff summary

- Commits: current `bd-c9b855` implementation commit
- Files touched:
  - `crates/caco-tui/src/app.rs`
- Tests:
  - `cargo test -p caco-tui summaries --lib` — passed
  - `cargo test-small` — 256 passed
- Behavioural delta: terminal users can quickly reset summaries filter input without leaving the view.

## Operator-takeaway

The TUI summaries filter now behaves more like a comfortable terminal input: `Ctrl-U` clears the current search instantly.
