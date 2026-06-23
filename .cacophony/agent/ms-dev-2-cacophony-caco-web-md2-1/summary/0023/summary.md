# Session summary — caco-web version-tag keyboard operability (a11y)

## Goal

Fix a keyboard/screen-reader accessibility gap found by the caco-web duty-cycle
keyboard-operability probe: the sidebar version tag is a click-to-copy control
but was not operable by keyboard (WCAG 2.1.1 Keyboard, 4.1.2 Name/Role/Value).

## Bead(s)

- `bd-273f2d` — caco-web version tag click-to-copy is not keyboard operable.

## Before state

- Failing tests: none. `.version-tag` (`#version-tag`, sidebar header) had
  `title="… click to copy"` + `onclick` (copies version + toast) but was a bare
  `<span>`: cursor:pointer, tabindex=null (not Tab-reachable), no role, no
  aria-label, no keydown — keyboard/SR users could not invoke the copy.

## After state

- Failing tests: none. `node --check` OK; `cargo test -p caco-web --lib` passed.
- `static/app.js` `renderVersionTag`: add `role="button"`, `tabindex="0"`,
  `aria-label="Copy version v<x>"`, and an `onkeydown` (Enter/Space → same copy
  as onclick). Verified: focusable=true, attrs set (chromium).

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files: `crates/caco-web/static/app.js` (`renderVersionTag`).
- Tests: +0 (JS; the `.version-tag` tests assert the CSS rule, not the JS — no
  conflict; caco-web lib tests green).
- Behavioural delta: the version tag is now keyboard-operable + announced as a button.

## Embedded artefacts

- `web/screenshots/version-tag-keyboard-focus.png` — version tag with keyboard focus.

## Operator-takeaway

A keyboard-operability probe (distinct from focus-ring/contrast checks) found the
click-to-copy version tag was mouse-only — now keyboard-operable and screen-reader
announced. Same WCAG-2.1.1 class as a click-only control; the SPA's interactive
chrome is otherwise keyboard-reachable. (Note: the beads service flaked 503 mid-cycle
and persisted a duplicate bead bd-804939, closed as a dup.)
