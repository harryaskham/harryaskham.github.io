# Session summary — bd-20b653: autocomplete=off for 6 selects

## Goal
Pattern (m) bd-06a1c2 autocomplete-hygiene extension to `<select>`: 6 selects lacked autocomplete=.

## Bead
- `bd-20b653`

## Audit
- All `<select>` scanned (HTML+JS, comments stripped).
- Yield: 6 candidates without autocomplete=.

## Fix
All 6 → `autocomplete="off"`:
- app.js move-bead-project-select, anonymous voice-select.
- timeline.js timeline-window, timeline-refresh-interval, timeline-granularity.
- workspace.html workspace-agent-select.

## Pattern parity (bd-06a1c2)
- Identifier text inputs: full hygiene.
- Prose textareas: off + sentences + spellcheck.
- `<select>`: off only.

## Regression test (~70 lines)
- 6 cases with attribute-anchor inside `<select ... >`.
- Walk-back: rfind `<select` from anchor; forward-find `>`.
- Assert tag contains `autocomplete="off"`.

## Operator-visible effect
- No browser autofill suggestions on these selects.

## Diff summary
- `crates/caco-web/static/app.js` -- 2 selects gain attr.
- `crates/caco-web/static/timeline.js` -- 3 selects gain attr.
- `crates/caco-web/static/workspace.html` -- 1 select gains attr.
- `crates/caco-web/src/tests.rs` -- new bd-20b653 forward-guard (~70 lines).
- Net pass: 573 -> 574; 0 failures.

## Operator-takeaway
65 cycles, 108 wins. Pattern (m) autocomplete extension to selects. Pattern catalog: 22 entries.
