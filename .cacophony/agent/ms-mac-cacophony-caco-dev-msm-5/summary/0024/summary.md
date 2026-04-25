# Session summary — Web summaries selected-row affordance

## Goal

Continue polishing caco-web summaries long-history usability by making the currently selected row easier to find again after detail interactions and keyboard navigation.

## Bead(s)

- `bd-a3ba00` — Web summaries: add visible selected-row persistence and back-to-list affordance
- related: `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android

## Before state

- Web summaries had keyboard row selection and a mobile `Show in list` affordance.
- The selected row was highlighted, but long-history/detail interactions could still make it easy to lose visual context.
- Keyboard navigation scrolled the selected row into view directly in the key handler, while the mobile jump duplicated similar logic.

## After state

- Selection changes can now request a shared `revealSelectedRow` helper that scrolls the row into view, optionally focuses it, and applies a temporary reveal pulse.
- Keyboard navigation uses the shared reveal helper without stealing focus.
- The mobile `Show in list` action uses the same helper and focuses the row for accessibility.
- The selected row styling is stronger: gradient background, border, selected badge, and reveal animation.

## Diff summary

- Commits: current `bd-a3ba00` implementation commit
- Files touched:
  - `crates/caco-web/static/summaries.js`
  - `crates/caco-web/static/summaries.css`
- Tests:
  - `node --check crates/caco-web/static/summaries.js` — passed
  - `cargo test-small` — 256 passed
- Behavioural delta: the web summaries list now keeps selection context much more visible during keyboard and mobile detail/list workflows.

## Operator-takeaway

The web viewer is now better for long histories: after drilling into detail, operators can confidently jump back to the selected row and see a clear visual anchor rather than hunting through the list.
