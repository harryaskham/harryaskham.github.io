# caco-web duty-cycle audit — Choices empty-state

## Broad pass (1440x900 + mobile 390x844)
Routes visited: status, agents, beads, feed, chat, nodes, services, projects,
choices, notifications, actions, logs, timeline, summaries, merge-queue.
- Console errors/warnings: 0 (console.json)
- Page errors: 0 (pageerrors.json)
- Network failures (>=400 / failed): 0 (network.json)
- Visual: Services, Projects, Actions, Merge Queue, Nodes (desktop + mobile)
  render cleanly. bd-4ca124 "last known / expected offline" node annotations
  confirmed rendering (po2/sgu EXPECTED OFFLINE, italic "last known" counts).

## bd-6215c2 (stale-as-live Agents/RUNNING headline) — left for triage
Confirmed the documented "prefer daemon-root" conclusion still holds: app.js
agent render has no node-reachability; the only app-wide map (state.nodesHealth)
carries peer_health only, not a clean liveness lookup. Unreachable-ish nodes on
this fleet render as MISMATCH (nodeStatusClass -> 'discarded'), not 'down', so
even the bd-4ca124 "last known" annotation does not key on them. Cross-cutting +
headline-metric + Harry offline => not a safe unilateral web slice. No change.

## Defect found + fixed: bd-f20f5a (Choices empty state)
The Choices view DEFAULTS to the `pending` filter, so renderChoices() computed
isFiltered = !!statusFilter = always true on the default landing, rendering the
generic search-no-results emptyState('search', 'No pending choices', { hint:
'Try clearing the status filter' }). An operator all caught up (the healthy
state) was told to clear a filter they never set. Fixed: default pending-empty
=> calm caught-up emptyState('check', ...) with a reassuring hint; resolved/
unavailable filters keep the "clear the filter" treatment; "All" keeps the rich
emptyStateRich('choices-none'). Verified live across all four filter states.

Evidence: screenshots/desk-choices.png + choices-list-only.png (before),
screenshots/choices-empty-after.png + desk-choices-after.png (after).
