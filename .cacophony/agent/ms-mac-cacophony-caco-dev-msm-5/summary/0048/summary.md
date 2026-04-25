# Session summary — Web summaries detail retry action

## Goal

Continue web summaries resilience polish by making selected-detail load failures recoverable without requiring a full page or list refresh.

## Bead(s)

- `bd-b784ed` — Web summaries: add retry action for failed detail loads
- related: `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android

## Before state

- The web summaries list had live loading/error states and explicit list refresh controls.
- If loading the selected parsed summary detail failed, the detail pane showed only an error message.
- Operators had to change selection, refresh the whole summaries view, or reload the page to retry the failed detail request.

## After state

- Detail-load errors now render as an alert with a `Retry detail` button.
- Retrying clears the selected detail error/cache entry, calls the existing detail loader for the selected key, and re-renders the pane.
- The action is scoped to the selected summary, so the rest of the loaded list remains stable.

## Diff summary

- Commits: current `bd-b784ed` implementation commit
- Files touched:
  - `crates/caco-web/static/summaries.js`
- Tests:
  - `node --check crates/caco-web/static/summaries.js` — passed
  - `cargo test-small` — 256 passed
- Behavioural delta: failed selected-detail fetches in web summaries now have an inline recovery path.

## Operator-takeaway

The web summaries detail pane is less of a dead end during transient daemon/API errors: operators can retry just the failed detail load while preserving list context.
