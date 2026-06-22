# Session summary — bd-4ca124: Nodes view stale-as-live counts for UNREACHABLE nodes

## Goal

Harry flagged (2026-06-22) that winmini/aurora/pocket4/sonance are offline by
design while he travels, and "we should be resilient to offline nodes." I audited
the caco-web Nodes view's offline-node rendering. The core resilience contract is
met (UNREACHABLE badge, Last-seen field, offline nodes don't break others), but
found one stale-as-live gap: down nodes showed last-known agent counts as live
"running".

## Bead(s)

- `bd-4ca124` — caco-web: Nodes view presents UNREACHABLE nodes' last-known agent
  counts as live "running" (filed + claimed + closed this session).

## Before state

- Live Nodes-view probe: "aur UNREACHABLE aurora · 18 agents · 18 running",
  "po4 UNREACHABLE pocket4 · 5 agents · 5 running", etc. A powered-off node cannot
  have verifiably-RUNNING agents — those counts are last-known, but were rendered
  identically to a live node's running count (SPEC "stale data presented as live").

## After state

- For down/unreachable nodes (`statusClass === 'down'`), the running count in both
  the subnav summary (nodes.js:306) and the detail card (nodes.js:229) is wrapped
  in a `.node-stale-count` span (muted + italic) with a "Last known before this
  node became unreachable — not verified live" tooltip; the detail adds an explicit
  ", last known". Reachable nodes — INCLUDING config-MISMATCH nodes (drifted but
  live) — render unchanged. Live probe confirmed: all 6 UNREACHABLE nodes get the
  stale-count class; all reachable nodes (SELF/MATCH/MISMATCH) do not.

## Diff summary

- Code commit: pending (final landed squash SHA from the reintegration receipt).
- Files: crates/caco-web/static/nodes.js (down-node count treatment at 2 render
  points), crates/caco-web/static/style.css (.node-stale-count), crates/caco-web/
  src/tests.rs (needle guard). CSS+JS only.
- Tests: +1 needle guard (nodes_unreachable_stale_count_treatment_bd_4ca124).

## Operator-takeaway

Directly serves Harry's offline-node-resilience point: the Nodes view already
rendered offline nodes gracefully (UNREACHABLE badge + Last-seen), but it asserted
their stale last-known agent counts as live "running" — so a glance at "aurora · 18
running" could imply a powered-off node was doing work. Now down nodes' counts are
visibly last-known. Key nuance: the treatment is gated on genuinely-down nodes, NOT
config-MISMATCH nodes (which are reachable + live), so it doesn't over-dim live drift.
