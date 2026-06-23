# Session summary — caco-web login overlay focus trap (a11y)

## Goal

Fix a keyboard-accessibility defect surfaced by the caco-web duty-cycle focus
probe: the login overlay (`showLoginForm`) declared `role=dialog aria-modal=true`
but let Tab escape to the obscured dashboard behind it. Add a focus trap so the
modal contains keyboard focus (WCAG 2.4.3 + the aria-modal contract).

## Bead(s)

- `bd-562f8a` — caco-web login overlay (showLoginForm) does not trap focus.

## Before state

- Failing tests: none. Chromium probe: trigger `showLoginForm()` → Tab ×12 →
  11/12 tabs escaped the overlay (focus moved to body / nav links / stat-cards).
  Main-dashboard focus rings were otherwise clean (40/40 focusable had a ring).

## After state

- Failing tests: none. `node --check app.js` OK. Chromium re-test: 0/14 tabs
  escape; focus cycles input ↔ submit; Shift+Tab also stays trapped.
- `static/app.js` `showLoginForm`: added a Tab/Shift+Tab keydown handler on the
  overlay that wraps focus within its focusable elements. ESC still does not
  dismiss (login is non-dismissible).

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files: `crates/caco-web/static/app.js` (focus trap in `showLoginForm`).
- Tests: +0 (JS-only; validated by `node --check` + chromium focus probe).
- Behavioural delta: the login modal now contains keyboard focus.

## Embedded artefacts

- `web/screenshots/login-focus-trap-fixed.png` — overlay after the trap fix.

## Operator-takeaway

The login overlay (from the auth arc) was the one modal that didn't trap focus —
a keyboard user filling the sign-in form could Tab into the dimmed dashboard
behind it. Now fixed to match the other modals' focus-trap behavior. Found via
the duty-cycle keyboard/focus probe; the main-dashboard focus indicators are
otherwise solid.
