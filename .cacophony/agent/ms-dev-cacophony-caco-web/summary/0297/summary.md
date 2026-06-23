# Session summary — caco-web auth-bootstrap profile fix + bd-4021f7 live confirmation

## Goal

After the node settled, do a fresh live observation pass (the healthy environment
I lacked during the storm). This surfaced a high-value workflow blocker: the
caco-web auth-default-on flip now breaks the observation workflow, and the
profile's launch guidance was stale ("prefer the node token"). Fix the profile so
future caco-web cycles aren't blocked, and post-hoc confirm bd-4021f7 live.

## Bead(s)

- No implementation bead (profile self-improvement + live verification of the
  already-landed bd-4021f7). Remaining draft: bd-0860c5 (empty-state audit).

## Before state

- The caco-web dashboard auth-default-on flip (bd-323724 / bd-e5f22f) landed during
  the storm: /api/v1/ui/snapshot now returns 401 and the shell renders empty
  (Agents 0) without a session. My observation servers were unauthenticated, so
  every probe saw empty panes — easy to mis-read as degraded daemon data (I nearly
  did during the bd-4021f7 cycle).
- The profile's Local Launch Workflow said "the dashboard should prefer the node
  token" — STALE/WRONG after the flip: the node token returns 401; the dashboard
  uses its own auto-generated ~/.cacophony/tokens/caco-web.token.

## After state

- Auth solved + documented: the dashboard token is ~/.cacophony/tokens/caco-web.token
  (loopback zero-config bootstrap). Browser/Playwright: navigate once to
  /?token=$(cat ~/.cacophony/tokens/caco-web.token) (SPA exchanges it for a
  caco_web_session cookie via /api/web/session). curl: Authorization: Bearer with
  the same token. Verified live: snapshot 200, 96 agents, dashboard populated.
- bd-4021f7 CONFIRMED live with auth working: Workspace Beads pane at 736px compact
  now has hOverflow 0, Labels/Owner/Updated hidden, visible columns
  ID/P/Status/Title/Actions (screenshots/ws-beads-compact-confirmed.png).
- .cacophony/profiles/caco-web.md: corrected the stale node-token guidance and
  added a "Dashboard auth (auth-default-on flip)" section with the concrete
  ?token / Bearer bootstrap commands. Alerted sibling caco-web-md2-0 via msg.

## Diff summary

- Code/content commit: pending (final landed squash SHA from the reintegration receipt).
- .cacophony/profiles/caco-web.md — corrected stale node-token note + added the
  Dashboard auth bootstrap section. Profile-only .md change (gate auto-skips).
- No app code change this cycle (bd-4021f7 already landed at 60a767dff4; this is
  its live confirmation + a workflow-doc fix).

## Embedded artefacts

- web/screenshots/ws-beads-compact-confirmed.png — bd-4021f7 live: beads pane
  compact, 0 overflow, secondary columns collapsed, ID/P/Status/Title/Actions
  visible.

## Operator-takeaway

The caco-web auth-default-on flip silently broke the observation workflow: an
unauthenticated dashboard renders empty (Agents 0 / 401 snapshots) and looks like
degraded daemon data. The fix is to authenticate with the dashboard's own
~/.cacophony/tokens/caco-web.token (NOT the node token) via the ?token SPA
bootstrap or a Bearer header. Captured in the profile + relayed to the sibling
caco-web agent so the whole caco-web lane stops losing observation cycles to the
401 wall. Also post-hoc confirmed bd-4021f7 works live (beads pane 0 overflow).
