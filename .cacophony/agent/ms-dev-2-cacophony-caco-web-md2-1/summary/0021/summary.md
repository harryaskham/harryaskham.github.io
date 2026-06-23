# Session summary — caco-web profile: gate-discipline for echo-gated caco-web lands

## Goal

Capture a workflow lesson (caco-ctrl process reminder, 2026-06-23) in the caco-web
profile: always run `cargo test -p caco-web --lib` before landing ANY caco-web
change — including JS-only static edits — because this agent is echo-gated (no
test-small) but caco-web tests gate every cacophony-fast-tests agent.

## Bead(s)

- (no implementation bead) — profile/operational maintenance from the caco-ctrl
  fleet-wide gate-discipline reminder.

## Before state

- Failing tests: none. The profile's Validation section listed
  `cargo test -p caco-web --lib` but did not explain WHY it is mandatory for
  echo-gated caco-web lands or that JS-only edits also need it. UI test-assertion
  drift re-blocked the cacophony gate 3x on 2026-06-23.

## After state

- Failing tests: none.
- `.cacophony/profiles/caco-web.md` Validation section: added a "Gate discipline"
  note — run `cargo test -p caco-web --lib` before landing any caco-web change
  (incl. `static/*.js`/`*.html`), since the echo gate skips test-small but the
  caco-web crate tests (style_css_*, app_js_*, index_html_*) gate cacophony-fast-
  tests agents; node --check + Playwright validate JS but not the Rust assertions.

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files: `.cacophony/profiles/caco-web.md` (Validation section clarification).
- Tests: +0 (profile doc).
- Behavioural delta: none (guidance only).

## Operator-takeaway

Self-improvement: codified the echo-gate trap that re-blocked the fleet gate 3x
tonight — a caco-web change can pass the echo gate yet break a test-small caco-web
test and block every cacophony-fast-tests reint. The profile now mandates the
caco-web lib tests for every caco-web land, JS-only included. My own focus-trap
land tonight was verified green (in the gate's green run + a re-run), so this is
preventive, not a fix for a break I caused.
