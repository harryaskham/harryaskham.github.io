# Session summary — TUI summaries filter guidance

## Goal

Continue polishing the TUI summaries viewer after list filtering landed by making active-filter counts and no-match states clear enough that operators know what happened and how to recover.

## Bead(s)

- `bd-583969` — TUI summaries: show active filter match count and empty filter guidance
- related: `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android

## Before state

- The TUI summaries view supported `/` filtering over title, agent, project, and bead IDs.
- The active filter appeared in the title, but an empty filter result collapsed to a generic no-selection state.
- Operators could not immediately tell whether there were no summaries at all or no summaries matching the current filter.

## After state

- Active filters now reuse trimmed query text consistently in the outer title and list title.
- Filtered list titles explicitly say when the view is filtered and show selected/matched counts.
- Empty filtered results now render a dedicated guidance state in both list/detail space: the query, number of loaded summaries searched, match fields, and how to clear/refine/refresh.
- The generic no-summary state remains unchanged for truly empty datasets.

## Diff summary

- Commits: current `bd-583969` implementation commit
- Files touched:
  - `crates/caco-tui/src/views/summaries.rs`
- Tests:
  - `cargo test -p caco-tui summaries --lib` — passed
  - `cargo test-small` — 256 passed
- Behavioural delta: no API change; TUI filter UX now distinguishes no records from no filter matches and gives explicit recovery guidance.

## Operator-takeaway

The TUI summaries filter now feels less like a hidden debug shortcut: when a search returns nothing, the terminal explains what was searched and how to clear or adjust it.
