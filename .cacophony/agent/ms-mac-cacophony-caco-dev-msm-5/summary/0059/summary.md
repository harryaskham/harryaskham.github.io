# Session summary — Web summaries detail loading status

## Goal

Continue web summaries accessibility polish by ensuring the selected-detail loading placeholder is announced as status content.

## Bead(s)

- `bd-edbc38` — Web summaries: mark detail loading placeholder as status
- related: `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android

## Before state

- Web summaries detail errors used `role="alert"` and list loading states had explicit live/busy semantics.
- The selected-detail loading placeholder rendered visible text but had no status role.
- Assistive technology had less context when a selected row triggered a parsed-summary detail fetch.

## After state

- The detail loading placeholder now renders with `role="status"`.
- Existing text, loading flow, retry behavior, and detail actions remain unchanged.
- Screen-reader users get a clearer announcement while parsed summary details load.

## Diff summary

- Commits: current `bd-edbc38` implementation commit
- Files touched:
  - `crates/caco-web/static/summaries.js`
- Tests:
  - `node --check crates/caco-web/static/summaries.js` — passed
  - `cargo test-small` — 256 passed
- Behavioural delta: selected-detail loading now exposes status semantics in the web summaries view.

## Operator-takeaway

This is a focused accessibility polish: the web summaries detail pane now communicates loading progress as a status, matching the broader recovery/live-state work already done on the list.
