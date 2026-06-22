# Session summary — bd-fa3d05: page-title ordering parity

## Goal
Pattern (m) page-title ordering parity: notifications.html had inverted (brand-first) title.

## Bead
- `bd-fa3d05`

## Audit
- index.html: "Cacophony Dashboard" (canonical entry, no separator).
- workspace.html: "Workspace · Cacophony" ✓.
- notifications.html: "Cacophony · Notifications" ✗ (inverted).
- terminal.html: "Agent Terminal · Cacophony" ✓.

## Fix
- notifications.html: "Cacophony · Notifications" → "Notifications · Cacophony".
- index.html preserved as canonical-entry form.

## Why
- Browser tab truncation: leading word wins.
- Bookmark scannability: page-first.
- Slack/social link unfurl convention.

## Regression test (~70 lines)
- 3 middot-separated entry HTML; assert `<title>` starts with page name + contains "Cacophony".
- index.html: assert preserves canonical "Cacophony Dashboard".

## Operator-visible effect
- Notifications page browser tab shows "Notifications · Cacophony".

## Diff summary
- `crates/caco-web/static/notifications.html` -- 1-line title flip.
- `crates/caco-web/src/tests.rs` -- new bd-fa3d05 forward-guard (~70 lines).
- Net pass: 574 -> 575; 0 failures.

## Operator-takeaway
66 cycles, 109 wins. Pattern (m) page-title ordering parity. Pattern catalog: 22 entries.
