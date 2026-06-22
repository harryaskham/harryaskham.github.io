# Session summary — bd-cb07bf: JS-rendered table aria-label coverage

## Goal
Pattern (m) bd-735895 extension: 5 JS-rendered tables across app.js + workspace-*.js had no aria-label.

## Bead
- `bd-cb07bf`

## Audit
- 5 JS-rendered tables found across 4 JS files.

## Fix
All 5 tables gain aria-label:
- `keyboard-help-table` → "Keyboard shortcuts".
- `tts-per-agent-table` → "Per-agent TTS settings".
- `wbl-table` → "Beads".
- `ws-help-table` → "Workspace keyboard shortcuts".
- `ws-detail-table` → "Agent details".

## Why
- Region-wrapper labels don't propagate to table.
- Plain aria-label is non-visual.

## Regression test (~50 lines)
- Per (file, table-class, expected aria-label) tuple.
- Walk-back from class anchor to `<table` opening tag.
- Assert aria-label.

## Operator-visible effect
- "Beads, table, 6 columns" announcement instead of bare "table".

## Diff summary
- `crates/caco-web/static/app.js` -- 2 tables gain aria-label.
- `crates/caco-web/static/workspace-bead-list-pane.js` -- 1 table.
- `crates/caco-web/static/workspace-integrated.js` -- 1 table.
- `crates/caco-web/static/workspace-panes.js` -- 1 table.
- `crates/caco-web/src/tests.rs` -- new bd-cb07bf forward-guard (~50 lines).
- Net pass: 580 -> 581; 0 failures.

## Operator-takeaway
72 cycles, 115 wins. Pattern (m) JS table aria-label extension. Pattern catalog: 22 entries.
