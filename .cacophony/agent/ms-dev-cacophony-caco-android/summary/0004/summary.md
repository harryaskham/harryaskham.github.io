# Session summary — bd-379a50 WatchAgentsSummary destination

## Goal

Add a new global `WatchDestination.AgentsSummary` aggregate watch
destination calling `/api/v1/agents/summary`. Renders a watch-first leaner
glance focused on agents only: total + active counters, top by_state
counts, failed-agents short list with health cause, and duplicate-bead
warnings. Sibling of WatchFleetHealth but deliberately narrower.

## Bead(s)

- `bd-379a50` — Wearable uplift slice 72 — add Agents Summary aggregate
  watch destination (slice 72 in the bead title is the original spec
  number; that slot was reused by bd-5fd6dd, so the slice-history table
  lists this as slice 276).

## Before state

- The watch had `WatchDestination.FleetHealth` (bd-d6ef06) calling
  `/api/v1/agents/summary` and rendering the full health dashboard, but no
  leaner agents-only glance.
- The fetcher + bundle (`fetchWatchFleetHealth`, `WatchFleetHealthBundle`,
  `parseWatchFleetHealth`) already parsed every field needed for the
  agents-only view but no second screen consumed it.

## After state

- `companion/android/wearable/src/main/java/com/cacophony/companion/wear/agentssummary/WatchAgentsSummaryScreen.kt`
  (new): renders total + active + optional unreachable header, a `By state`
  pills card, and short lists for `Failed (N)` and `Dup beads (N)` capped
  at 5. Reuses the existing FleetHealth fetcher to avoid duplicating HTTP
  / parse code; deliberately omits `By node`, `recovery_backlog`, and
  `orphan_goal_agents` (those belong to FleetHealth, including them here
  would make this screen redundant).
- `companion/android/wearable/src/main/java/com/cacophony/companion/wear/nav/WatchNav.kt`:
  new `WatchDestination.AgentsSummary` and added to the `Primary` Home
  list directly after `FleetHealth` so the two agent-aggregate glances are
  adjacent in the Home menu.
- `companion/android/wearable/src/main/java/com/cacophony/companion/wear/MainActivity.kt`:
  new routing arm `WatchDestination.AgentsSummary -> WatchAgentsSummaryScreen(...)`.
- `companion/android/wearable/SURFACE.md`: new row `6c` Agents Summary
  documenting the contract and explicit-omits-by_node/recovery_backlog/
  orphan_goal distinction from row `6b` Fleet Health; slice-history row
  `276` for bd-379a50.
- New `WatchAgentsSummarySourceTest`: six tests pinning the destination
  declaration + Primary membership + adjacency to FleetHealth, MainActivity
  routing, screen reuses the existing fetcher, screen renders the agents-
  focused sections, screen omits the FleetHealth-only sections, the parser
  round-trips the fields the new screen depends on, and SURFACE.md
  documents the new rows.

## Diff summary

- Code commit: pending final squash SHA from reintegration receipt.
- Files touched (4):
  - `companion/android/wearable/src/main/java/com/cacophony/companion/wear/agentssummary/WatchAgentsSummaryScreen.kt`
    (new)
  - `companion/android/wearable/src/main/java/com/cacophony/companion/wear/nav/WatchNav.kt`
  - `companion/android/wearable/src/main/java/com/cacophony/companion/wear/MainActivity.kt`
  - `companion/android/wearable/SURFACE.md`
- Tests: +6 unit tests in
  `companion/android/wearable/src/test/java/com/cacophony/companion/wear/WatchAgentsSummarySourceTest.kt`.
- Behavioural delta: Home menu now has an Agents Summary destination
  directly after Fleet Health. Tapping it renders a leaner agents-only
  dashboard at /api/v1/agents/summary. Existing Fleet Health remains
  unchanged.

## Embedded artefacts

- None this session.

## Operator-takeaway

Agents Summary is the watch-first equivalent of "show me agents totals,
state breakdown, failures, and dup-bead warnings only" without the
broader fleet-health context. Two destinations now consume
/api/v1/agents/summary on the watch: Fleet Health (the full dashboard
with by_node / recovery_backlog / orphan_goal_agents) and Agents Summary
(the agents-only glance). One daemon endpoint, one fetcher, two
purpose-built screens.
