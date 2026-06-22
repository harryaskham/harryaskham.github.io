# Session summary — caco-web Workspace footer choices-count consistency (bd-0bb063)

## Goal

Fix an operator-trust inconsistency: the caco-web Workspace footer showed
"✅ no choices" while an operator choice was genuinely active and the sidebar
flagged it urgently ("Choices ⚠1"). Make the footer's active-choice count agree
with the sidebar so an actionable operator decision is never hidden in one
surface while urgent in another.

## Bead(s)

- `bd-0bb063` — caco-web: Workspace footer shows 'no choices' while an operator
  choice is active (sidebar shows it urgently) — operatorInbox short-circuit
  (filed + claimed + implemented this cycle).

## Before state

- Failing tests: none known.
- Daemon truth (caco 1.2.1264): `caco choices list --status active` = 1 active
  choice (choice-019ed6e4…, ms-dev-2-android-utils, 3 options, 48m old).
- Sidebar: `nav-item-choice-urgent` + `urgent-choice` badge "!1" (correct).
- Workspace footer `#ws-status-choices`: "✅ no choices" (wrong).
- Probe: `window.state.operatorInbox` had 1 active choice row;
  `window.pendingChoiceCountFromState()` returned 1.

## After state

- Failing tests: none known. `node --check workspace-integrated.js` OK;
  `updateStatusBar` + `ws-status-choices` intact (workspace_has_status_bar test);
  `git diff --check` clean.
- `updateStatusBar()` now counts active choices like the sidebar's
  `pendingChoiceCountFromState()`: pending `s.choices` UNION active
  `s.operatorInbox` rows (kind==='choice' && status==='active'), deduped.
- Live logic validation against `window.state`: old expression → 0
  ("no choices"); new logic → 1 ("🤔 1 choice", alert), matching the sidebar.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-web/static/workspace-integrated.js`
  (updateStatusBar choice computation).
- Tests: +0 / -0 (frontend logic fix; validated by live old-vs-new simulation +
  test-constraint re-check).
- Behavioural delta: Workspace footer choice count now matches the sidebar /
  daemon active-choice set instead of short-circuiting on an empty-but-truthy
  `s.choices` array.

## Embedded artefacts

- `web/screenshots/ws-01.png` — Workspace 2-pane layout.
- `web/screenshots/ws-footer-before.png` — footer "✅ no choices" (bug).
- `web/screenshots/ws-footer-after.png` — footer "🤔 1 choice" (corrected
  rendering demonstrated by patching the element).

## Operator-takeaway

`(s.choices || s.operatorInbox || [])` silently ignores `s.operatorInbox`
whenever `s.choices` is an empty array (truthy) — a classic JS `||`-on-array
trap. Because active operator choices arrive as operatorInbox rows, the footer
under-reported them and could tell an operator "no choices" while a real
decision was pending. The canonical count lives in
`pendingChoiceCountFromState()`; any other surface counting choices should mirror
its union-of-sources logic rather than re-deriving from one field.
