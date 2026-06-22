# Session summary — bd-51182b: Pico conversation density collapse (web surface)

## Goal

Implement the P2 feature: a collapse-all / density toggle for the caco-web Pico
conversation display, so long transcripts can be condensed to one-line previews for
scanning + scroll performance, with a 3-mode cycle (Expanded / Condensed [only
direct user<->assistant messages expanded] / Collapsed). bd-3bcc83 is the mobile
sibling.

## Bead(s)

- `bd-51182b` — Add collapse all message bubbles feature to web surface (filed by
  node-token; claimed + implemented + closed this session). Sibling: bd-3bcc83
  (mobile, separate).

## Before state

- The Pico transcript rendered every bubble (user/assistant/thinking/tool/note)
  fully expanded with no density control; long sessions required heavy scrolling
  with no way to condense.

## After state

- WEB (app.js): picoState.collapseMode ('full'/'condensed'/'collapsed') +
  expandedItems; renderPicoSnapshot tags each transcript bubble with data-pico-idx
  (picoApplyCollapse) and sets a host density class; a footer density control
  (onclick picoCycleCollapseMode) cycles the 3 modes; picoPanelClick lets the
  operator click a collapsed bubble to individually expand/re-collapse it.
- WEB (style.css): .pico-collapse-collapsed truncates every bubble to its role +
  one-line preview; .pico-collapse-condensed truncates all EXCEPT direct
  user/assistant bubbles; .pico-expanded overrides; .pico-foot-density control.
- TEST (observe.rs): PICO_COLLAPSE_ASSERT_EVAL live subscenario cycles the modes
  and asserts via computed max-height that full collapses nothing, condensed keeps
  direct bubbles full, collapsed collapses them, and cycling returns to full
  (pico-pane scenario now 61 results, collapse subscenario green).

## Diff summary

- Code commit: pending (final landed squash SHA from the reintegration receipt).
- Files: crates/caco-web/static/app.js (state + render + control + click),
  crates/caco-web/static/style.css (density CSS), crates/caco-web/src/bin/
  caco-web-observe.rs (live subscenario), crates/caco-web/src/tests.rs (needle
  guard). No wasm change (pure web view concern).
- Tests: +1 needle guard, +1 live observe subscenario (61 results, verified green).

## Operator-takeaway

The Pico web conversation now has a 3-mode density toggle so long transcripts
condense to scannable one-line previews (Condensed keeps the actual user<->agent
exchange readable while folding thinking/tool/note noise; Collapsed folds
everything; click any collapsed bubble to peek). Verified end-to-end with a live
Playwright subscenario asserting the computed collapse per mode. The mobile sibling
(bd-3bcc83) remains for the mobile surface.
