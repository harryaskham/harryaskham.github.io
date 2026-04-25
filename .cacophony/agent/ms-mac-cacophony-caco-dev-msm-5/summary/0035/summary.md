# Session summary — Web summaries clear-all filters

## Goal

Continue caco-web summaries polish by making multi-filter recovery faster when project, agent, and bead filters are stacked.

## Bead(s)

- `bd-837801` — Web summaries: add clear-all filters action
- related: `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android

## Before state

- Active filters were shown as individually clearable chips.
- Clearing a stacked filter state required clicking each chip one at a time.
- There was no visually distinct reset action for returning to the unfiltered summary list.

## After state

- When any filter is active, the active-filter strip now includes a `Clear all` action chip.
- `Clear all` resets project, agent, and bead filters together and reloads from offset zero.
- The reset chip has a distinct red-tinted treatment and explicit accessible label.

## Diff summary

- Commits: current `bd-837801` implementation commit
- Files touched:
  - `crates/caco-web/static/summaries.js`
  - `crates/caco-web/static/summaries.css`
- Tests:
  - `node --check crates/caco-web/static/summaries.js` — passed
  - `cargo test-small` — 256 passed
- Behavioural delta: operators can recover from stacked web summary filters with one click.

## Operator-takeaway

The web summaries filter bar now has an obvious escape hatch: one `Clear all` chip resets every active filter at once.
