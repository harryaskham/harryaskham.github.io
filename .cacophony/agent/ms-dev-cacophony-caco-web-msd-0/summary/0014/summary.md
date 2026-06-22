# Session summary — caco-web: make web/daemon version drift visible

## Goal

Run a fresh caco-web dashboard duty cycle on ms-dev (Linux/nix node) to
establish a baseline across every operator-visible surface, then turn the first
concrete operator-trust defect that surfaced into a landed fix. The cycle ran a
comprehensive desktop pass (all 22 views), a corrected mobile pass (390px), and
a focused version-provenance probe, then fixed the version-drift visibility gap.

## Bead(s)

- `bd-2d5b92` — caco-web: version drift (web vs daemon) only shown in tooltip,
  not visibly (bd-cb6576 follow-up) [filed + claimed + fixed this cycle]
- Related exemplar: `bd-cb6576` — version/build provenance mismatch (the class
  of operator-trust bug this fix closes one more instance of).
- Not touched (already owned): `bd-8e64f0` (md-body/pico-body-md inline-code
  overflow, in_progress under ms-dev:harryaskham); `bd-c1504b` (operator-action,
  skipped per auto-claim policy).

## Before state

- Failing tests: none observed.
- Desktop pass (chromium 148 via playwright-core, 1280x900, all 22 views):
  0 console errors, 0 page errors, 2 benign SSE `ERR_ABORTED` on teardown
  (`/api/v1/ui/stream`, `/api/v1/logs/stream`), and only the beads `data-table`
  extending past the viewport inside its own horizontal-scroll wrapper
  (document scrollWidth stayed 1280 — by design).
- Mobile pass (390x844, navigated via location.hash): 0 document-level
  horizontal overflow and 0 unclipped offenders across all 22 views.
- Version provenance: `#version-tag` rendered `v1.2.1333` in muted gray
  (rgb(123,136,161), 10px). In `renderVersionTag()` (static/app.js ~2727) the
  only drift handling appended ` (daemon vX)` to `tag.title` (the tooltip).
  Visible badge text/styling never changed on drift — the exact bd-cb6576
  anti-pattern ("hiding it in an ambiguous tooltip").

## After state

- Failing tests: none (static-only JS/CSS change; cacophony-fast-tests gate runs
  cargo check/test-small/clippy on reintegration and is unaffected by static
  assets).
- `renderVersionTag()` now computes `hasDrift` and, when the caco-web shell and
  daemon builds diverge, renders the badge text as `v<web> · daemon v<daemon>`,
  toggles a `.version-tag-drift` class (amber `--warning`, weight 600), and uses
  an explanatory tooltip ("...assets may be stale — reload after the web service
  updates"). No-drift path is unchanged (`v<web>`, muted).
- Validated end-to-end through the real JS path in chromium:
  - no-drift: text `v1.2.1333`, color rgb(123,136,161), weight 400, class
    `version-tag`.
  - forced drift (`state.daemonVersion='1.2.1300'; renderVersionTag()`): text
    `v1.2.1333 · daemon v1.2.1300`, color rgb(235,203,139) (= --nord13 warning),
    weight 600, class `version-tag version-tag-drift`; fits one sidebar line, no
    clipping.

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-web/static/app.js` (renderVersionTag drift
  branch), `crates/caco-web/static/style.css` (new `.version-tag-drift` rule).
- Tests: +0 / -0 / flipped 0 (static asset change; validated by live browser
  probe rather than a unit test).
- Behavioural delta: web/daemon version drift is now visible in the sidebar
  badge (text + warning color) instead of being hidden in the hover tooltip.

## Embedded artefacts

- `web/screenshots/version-drift-header.png` — sidebar crop showing
  `v1.2.1333 · daemon v1.2.1300` in the amber drift state.
- `web/screenshots/version-drift-after.png` — full status view with the drift
  badge.
- `web/screenshots/status-1280.png`, `beads-1280.png`, `summaries-1280.png` —
  representative clean desktop surfaces.
- `web/screenshots/m-status.png`, `m-beads.png`, `m-chat.png`, `m-summaries.png`,
  `m-timeline.png` — representative valid mobile (390px) surfaces.
- `web/observe-report.json` — desktop console/network/overflow capture.
- `web/mobile-overflow.json` — per-view mobile horizontal-overflow probe.

## Operator-takeaway

caco-web already tracked web vs daemon build versions separately, but only
exposed drift in a tooltip — so a stale dashboard shell serving old embedded
assets against a freshly-updated daemon looked identical to a healthy one. The
sidebar now shows both versions in an amber drift state when they diverge, so
operators can see "reload your dashboard" at a glance. The rest of the dashboard
(desktop + mobile, all 22 views) was console-clean and overflow-clean this pass.
