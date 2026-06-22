# Session summary — caco-web: activate 'expected offline' rendering via bulk nodes endpoint

## Goal

Complete bd-0e7c97 (caco-web offline-node "expected offline" rendering), which
had landed but was INERT against live data. caco-ctrl flagged that a 1339 daemon
(which includes bd-03fad6) was returning peer_health:null; investigating that
surfaced the real cause and a clean in-lane web fix that activates immediately.

## Bead(s)

- `bd-09624d` — caco-web: 'expected offline' rendering inert (peer_health only on
  NodeSummary/list, not NodeDetail); read from bulk /api/v1/nodes. [this session]
- Completes `bd-0e7c97` (rendering scaffold landed at 58a3e02832 but inert).
- Daemon enabler `bd-03fad6` (added peer_health to NodeSummary only).

## Before state

- caco-web nodeLivenessBadge() read peer_health from the per-node DETAIL fetch
  (/api/v1/nodes/<name> -> NodeDetail). bd-03fad6 added peer_health to
  NodeSummary (LIST /api/v1/nodes) ONLY, not NodeDetail. So info.peer_health was
  always undefined and the calm "expected offline" badge never rendered against
  live data (the prior injection test masked this).
- Live proof (daemon 1.2.1339 = 2a7bec7ec, includes bd-03fad6): /api/v1/nodes
  winmini entry HAS peer_health{expected:true}; /api/v1/nodes/winmini has none.

## After state

- nodes.js: new fetchNodesHealth() GETs the bulk /api/v1/nodes (which already
  carries peer_health on current daemons), builds state.nodesHealth
  {name -> peer_health}; nodeLivenessBadge() reads
  state.nodesHealth[info.name] (fallback info.peer_health); renderNodes()
  triggers it alongside the per-node detail fetches.
- Live-validated (NO injection) against local daemon 1.2.1339: state.nodesHealth
  populated from real /api/v1/nodes; winmini + pocket4 render calm "expected
  offline" (degraded + operator note); ms-dev "self"/running with note tooltip;
  0 page errors. nodes.js content-assertion needles preserved; JS syntax OK.
- Activates immediately on current daemons (no daemon rebuild needed) since the
  LIST endpoint already serves peer_health.

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-web/static/nodes.js` (+28/-1): add
  fetchNodesHealth(); nodeLivenessBadge() reads bulk peer_health; renderNodes()
  triggers the bulk fetch.
- Tests: +0 / -0 (static asset; live-browser validated).
- Behavioural delta: the "expected offline" rendering now actually activates for
  config-annotated unreachable nodes (winmini/pocket4) on current daemons.

## Embedded artefacts

- `web/screenshots/nodes-live-expected-offline.png` — Nodes view rendered from
  LIVE daemon data: winmini/pocket4 "EXPECTED OFFLINE" (calm), no injection.

## Operator-takeaway

The offline-node calm rendering is now actually live (not just landed-but-inert):
caco-web reads expectation-aware peer_health from the bulk /api/v1/nodes endpoint,
which current daemons already serve, so winmini/pocket4 read calmly as "expected
offline" today without waiting for a daemon rebuild. Root cause was a real
asymmetry — bd-03fad6 added peer_health to NodeSummary (list) but not NodeDetail
(per-node) — which the earlier injection-only test masked. I flagged the
NodeDetail gap separately for daemon owners so macOS/TUI detail panes (which
consume NodeDetail) get the same data; caco-web no longer needs it.
