# Session summary — TUI summaries Ctrl-U help

## Goal

Continue TUI summaries polish by making the recently-added `Ctrl-U` filter clearing shortcut discoverable in the view itself.

## Bead(s)

- `bd-2046b5` — TUI summaries: document Ctrl-U filter clear
- related: `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android

## Before state

- The summaries slash filter supported `Ctrl-U` to clear the current query.
- The visible footer/help copy only mentioned `/`, `Esc`, and refresh, so users had to infer or already know the terminal-style clear shortcut.
- The no-match guidance told users how to refine or clear the whole filter but not how to clear just the query while staying in filter mode.

## After state

- The TUI summaries footer now includes `Ctrl-U clear query` next to the existing filter and refresh controls.
- The filtered empty-state guidance now explicitly mentions `Ctrl-U`, `Esc`, and `r` with distinct meanings.
- Behaviour remains unchanged; this is discoverability polish.

## Diff summary

- Commits: current `bd-2046b5` implementation commit
- Files touched:
  - `crates/caco-tui/src/views/summaries.rs`
- Tests:
  - `cargo test -p caco-tui summaries --lib` — passed
  - `cargo test-small` — 256 passed
- Behavioural delta: operators can now discover the TUI summaries query-clear shortcut without prior knowledge.

## Operator-takeaway

The TUI summaries filter now teaches both levels of recovery: `Ctrl-U` to clear the typed query and `Esc` to clear/leave the filter.
