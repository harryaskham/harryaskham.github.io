# Session summary — TUI summaries affordance polish

## Goal

Bring the TUI summaries pane up to the same usability bar as the web and Android summaries surfaces by making navigation and long-list state visible instead of implicit.

## Bead(s)

- `bd-1c0036` — Summaries TUI: align keyboard help and long-list affordances
- related: `bd-206809` — Summaries: cross-surface UX contract and polish checklist

## Before state

- TUI summaries supported basic j/k or arrow navigation, but the pane did not advertise those controls.
- The title only showed count/total, not selected row position.
- The list column had no explicit list title or selected-position count.
- Detail scrolling affordance was invisible.

## After state

- Outer title now shows selected position, shown count, and total count.
- Left list column title shows `List selected/total`.
- Added a footer help strip: j/k/up/down selection, PageUp/PageDown, g/G, r refresh, and detail scroll hint.
- Detail body now includes a dim tip explaining that mouse/trackpad scrolls the detail while j/k moves the list.
- Layout reserves a stable one-row help strip while preserving the two-column list/detail layout.

## Diff summary

- Commits: current `bd-1c0036` commit
- Files touched:
  - `crates/caco-tui/src/views/summaries.rs`
- Tests:
  - `cargo test -p caco-tui summaries --lib` — passed
  - `cargo test-small` — 252 passed
- Behavioural delta: no API changes; TUI summaries is clearer and easier to operate with long lists.

## Operator-takeaway

The terminal summaries surface now advertises how to drive it and where you are in the list, closing an important discoverability gap versus the web and Android viewers.
