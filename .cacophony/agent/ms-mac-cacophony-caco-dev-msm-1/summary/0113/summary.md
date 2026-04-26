# Session summary — project and scope control QA

## Goal

Continue the native macOS Tendril loop with a focused pass over project/scope controls, health/status chips, and related keyboard interactions, verifying whether these compact controls expose native menus or disabled/offline feedback.

## Bead(s)

- `bd-4defb0` — `[macOS visual QA] Continue full-surface Tendril polish loop`

## Before state

- Failing tests: none observed in this branch; the app build succeeded.
- Relevant metrics: fresh app launched from `/private/tmp/Cacophony-ms-mac-cacophony-caco-dev-msm-1-1777186085.app/Contents/MacOS/Cacophony`.
- Context: prior passes showed toolbar actions and command palette affordances were weak in offline state.

## After state

- Failing tests: none introduced; no product code changed.
- Relevant metrics: captured summaries `0111` and `0112`; filed `bd-bc7425`.
- Context: top project scope pill, health/status chips, and in-pane project/state/refresh chips did not open menus, pickers, popovers, or action-specific feedback; arrow/Enter did not visibly select scope.

## Diff summary

- Commits: `0fea85e49`, `HEAD`
- Files touched: `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-1/summary/0111/`, `0112/`, and this summary.
- Tests: +0 / -0 / flipped 0; visual QA artefacts only.
- Behavioural delta: no app behavior changed; evidence records project/scope control affordance gaps.

## Operator-takeaway

Project/scope controls currently read as interactive native controls but behave like static labels in the offline state. They need either real pickers/popovers or visibly disabled/offline affordances.
