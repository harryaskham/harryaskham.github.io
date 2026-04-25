# Session summary — Web summaries list empty-state actions

## Goal

Continue web summaries recovery polish by matching the Android empty-state direction: when the list is empty, filtered to nothing, or fails to load, the next action should be visible in-place.

## Bead(s)

- `bd-315097` — Web summaries: add list empty-state recovery actions
- related: `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android

## Before state

- The web summaries list had loading, error, and empty messages.
- List errors only displayed the error text.
- Empty filtered results were indistinguishable from a genuinely empty summaries history, and clearing filters required using the filter chips/header controls.

## After state

- List load errors now include a `Retry list` button.
- Empty filtered results now say `No matching summaries` and provide `Clear filters` plus `Refresh` actions.
- Genuinely empty summaries histories keep the recording-profile explanation and add a `Refresh` action.
- Empty-state actions are centered with a small responsive action row.

## Diff summary

- Commits: current `bd-315097` implementation commit
- Files touched:
  - `crates/caco-web/static/summaries.js`
  - `crates/caco-web/static/summaries.css`
- Tests:
  - `node --check crates/caco-web/static/summaries.js` — passed
  - `cargo test-small` — 256 passed
- Behavioural delta: web summaries list empty/error states now provide explicit retry, refresh, and clear-filter recovery paths.

## Operator-takeaway

The web summaries list is less of a dead end: every empty/error state now tells the operator what happened and offers a direct recovery action in the same place.
