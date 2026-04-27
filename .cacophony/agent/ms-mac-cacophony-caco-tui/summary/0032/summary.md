# Session summary — Ctrl-P opens views

## Goal

Make the global Ctrl-P quick-open picker useful for direct navigation to TUI views, especially the operator-reported example of typing `chat`.

## Bead(s)

- `bd-9b7a5a` — ctrl- picker should have all views included - ie ctrl-p "chat"

## Before state

- Failing tests: no test covered view entries in the fuzzy picker.
- Relevant metrics: not a performance change.
- Context: Ctrl-P searched entities such as projects, agents, beads, scratchpads, and profiles, but did not include first-class view destinations. Typing `chat` could select scratchpad creation rather than navigating to Chat.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: Ctrl-P now includes a `Views` section with global views and per-project views, including global `Chat` and per-project `<project> Chat`. Selecting a view switches the focused workspace content to that pane. The section order preserves existing project/agent selection behavior while making views win before scratchpad creation for view-like queries.

## Diff summary

- Commits: `8c29d4894`
- Files touched: `crates/caco-tui/src/views/fuzzy_picker.rs`, `crates/caco-tui/src/app.rs`
- Tests: +2 regression tests / -0 / existing picker tests preserved
- Behavioural delta: Ctrl-P can now navigate directly to common TUI views such as Chat, Audio, Status, Logs, Summaries, Timeline, and project-scoped views.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui fuzzy_picker --lib`

## Operator-takeaway

Ctrl-P now works as a true view picker: typing `chat` can open Chat directly instead of only searching data entities or offering scratchpad creation.
