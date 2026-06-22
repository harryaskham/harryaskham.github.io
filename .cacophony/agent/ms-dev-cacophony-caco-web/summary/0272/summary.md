# Session summary — bd-66018c: fix Pico dialog/model onclick single-quote breakout (correctness + XSS)

## Goal

Fix a P2 correctness + security bug found by reading renderPicoInteractivePanels:
select-dialog options and model-picker names were embedded inside inline onclick
JS string literals, so a single quote broke the handler (and could inject JS).

## Bead(s)

- `bd-66018c` — dialog/model-picker onclick breaks on single-quote values

## Before state

- Failing tests: none (untested edge). select option: onclick="answerPicoDialog(
  '<id>','value','<escapeAttr(o)>')"; model: onclick="choosePicoModel(
  '<escapeAttr(m)>')". escapeAttr turns ' into &#39;, but the HTML parser DECODES
  &#39; back to ' in the attribute value before the onclick JS runs. So an option
  like "don't deploy" produced a JS SyntaxError on click (button non-functional),
  and a crafted value like ',alert(1),' executed (JS injection; escapeAttr does
  not escape (),commas).

## After state

- Failing tests: none. The dialog/model buttons now carry their values in data-*
  attributes (data-pico-dlg / data-pico-kind / data-pico-value / data-pico-model)
  read via dataset (a plain-string context, NOT JS eval, so &#39; decodes to a
  safe string), dispatched by a single delegated click handler (picoPanelClick)
  bound once on the transcript host. No user data is embedded in inline onclick.
  New live subscenario asserts a "don't deploy" option round-trips exactly via
  data-pico-value, has no inline onclick, and is selectable (clicking clears the
  dialog). The existing confirm/select/cancel/input/model-picker subscenarios
  still pass via the delegated handler. 2/2 clean.
- caco-web bin 12; `--lib` 653; clippy clean.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/app.js` — data-* attributes + picoPanelClick delegation; bound once in renderPicoSnapshot.
  - `crates/caco-web/src/bin/caco-web-observe.rs` — single-quote dialog-option subscenario + eval.
- Tests: +1 live subscenario.
- Behavioural delta: dialog options / model names with single quotes now work and cannot inject JS.

## Embedded artefacts

- None.

## Operator-takeaway

Found by reading the dialog rendering: escapeAttr's &#39; is insufficient inside
an inline onclick JS string because HTML decodes it back to ' before the JS runs.
The robust fix is data-* attributes + event delegation (no user data in inline
JS). Worth a broader sweep for other inline-onclick-with-user-data patterns in
the dashboard.
