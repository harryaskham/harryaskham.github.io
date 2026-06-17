# Session summary — caco-web Workspace footer false "offline" connection segment

## Goal

Fix an operator-trust defect found during a caco-web Workspace probe: the
Workspace footer status bar permanently rendered an alert-styled "offline"
connection segment even while the dashboard was fully Connected and live —
directly contradicting the sidebar "Connected" / "Live data connected" state
(the bd-cb6576 / bd-b6ab99 connection-state-drift class).

## Bead(s)

- `bd-e94c3b` — caco-web Workspace footer permanently shows alert "offline" while daemon is Connected

## Before state

- Failing tests: none (static-asset logic bug).
- Workspace footer `#ws-status-conn` rendered `○ offline` with class
  `ws-status-segment ws-status-segment--alert` while the sidebar showed
  "Connected" (green) and "Live data connected · snapshot partial".
- Root cause (live Playwright + DOM probe on ms-dev-2): `updateStatusBar()` in
  `workspace-integrated.js` computed `isLive` from
  `#connection-status.classList.contains('connected'|'live')`, but
  `setConnectionStatus()` never puts a `connected`/`live` class on the
  `#connection-status` CONTAINER — it sets `state.connectionStatus='connected'`
  and adds `connected` to the child `.status-dot`. Verified the container class
  was exactly `"connection-status"` while connected, so `isLive` was ALWAYS
  false → the footer permanently showed the alert "offline".

## After state

- Failing tests: none. `node --check workspace-integrated.js` clean;
  `cargo test -p caco-web --lib` passed via the daemon test queue (exit 0).
- `updateStatusBar()` now derives `isLive = s.connectionStatus === 'connected'`
  (`s` is `window.state`, already in scope), matching the sidebar. Verified live:
  `#ws-status-conn` text = `● live`, class = `ws-status-segment--live`,
  `state.connectionStatus = connected`, console clean.
- The existing snapshot-timeout / snapshot-delayed / offline fallback labels for
  genuinely non-connected states are unchanged.

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-web/static/workspace-integrated.js` —
  `updateStatusBar()` connection segment `isLive` derivation only (one logical
  line + explanatory comment).
- Tests: +0 / -0; validated via DOM probe (text/class/connStatus) and before/after
  screenshots. caco-web lib tests remain green.
- Behavioural delta: the Workspace footer connection segment now reflects the
  real daemon connection state (live when connected) instead of a permanent
  false "offline" alert.

## Embedded artefacts

- `web/screenshots/ws-1440.png` — pre-fix Workspace at 1440px (footer shows
  alert "○ offline" while sidebar shows Connected).
- `web/screenshots/after-ws-conn-1440.png` — post-fix (footer shows "● live").

## Operator-takeaway

The Workspace footer no longer cries "offline" while the dashboard is connected —
it was a stale container-class check that could never be true. Found via a
Workspace probe (the surface my profile emphasizes), this is exactly the
operator-trust drift class (bd-cb6576/bd-b6ab99): an alarming status indicator
contradicting the actual connection. Also noted this cycle: bd-04d343
(caco-web-observe pico-pane flaky assertion) remains a well-documented in-surface
backlog item whose proper fix needs an async mock-harness change + load
reproduction, so it was deliberately not churned here.
