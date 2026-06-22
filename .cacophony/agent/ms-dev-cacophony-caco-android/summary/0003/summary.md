# Session summary — bd-ede6af Android agents inventory provenance

## Goal

Bring the Android companion in line with the macOS slice (bd-ad29d8 landed at
0cf4dbb81) so the agents inventory carries explicit daemon-owned-fleet-read-model
vs daemon-local-only provenance, and the AgentsListScreen surfaces that
provenance so client-local fallback rows cannot be mistaken for authoritative
fleet state.

## Bead(s)

- `bd-ede6af` — Android: bootstrap from daemon-owned fleet snapshot cache
  (Android slice of `bd-ad29d8`, parent `bd-480952`).
- Closed as duplicate in this session: `bd-482615`, `bd-b982a6` (same title,
  same parent, same description, filed within 4 seconds of each other in a
  quickfile burst on 2026-05-31T12:47).

## Before state

- Android `/api/v1/agents` consumers (snapshot + pull-sync paths in
  `AppStateStore`) parsed the rows but discarded the `inventory_mode` /
  `local_only` provenance fields, so client-local fallback was
  indistinguishable from daemon-owned fleet-latest output.
- The macOS slice at `0cf4dbb81` introduced `AgentsResponse.inventoryMode` /
  `localOnly` plus `isFleetLatest` / `isLocalOnly` / `inventorySourceLabel`
  helpers and rendered an `AgentsPane.agentInventorySourcePill`. Android
  had no equivalent.
- `bd-482615` and `bd-b982a6` duplicated `bd-ede6af` in the open queue.

## After state

- `companion/android/app/src/main/java/com/cacophony/companion/state/Models.kt`:
  new `AgentsInventoryProvenance` data class mirroring the macOS shape.
- `companion/android/app/src/main/java/com/cacophony/companion/state/AppStateStore.kt`:
  new `_agentsInventory` flow + parse-site updates in `handleSnapshot` and
  `handlePullSync /api/v1/agents`.
- `companion/android/app/src/main/java/com/cacophony/companion/ui/agents/AgentsListScreen.kt`:
  subscribes to `agentsInventory`, renders new `AgentsInventoryProvenancePill`
  composable below `HeroHeader`.
- New `AgentsInventoryProvenanceTest`: 10 unit tests covering default
  treatment, factories, JSON round-trips, AppStateStore source-pins, and
  AgentsListScreen integration source-pin.

## Diff summary

- Code commit: 67d1140fb1 (this branch) — pending final squash SHA from
  reintegration receipt.
- Closed duplicates this session: `bd-482615`, `bd-b982a6` (no code; admin
  override with reason).
- Files touched:
  - `companion/android/app/src/main/java/com/cacophony/companion/state/Models.kt`
  - `companion/android/app/src/main/java/com/cacophony/companion/state/AppStateStore.kt`
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/agents/AgentsListScreen.kt`
  - `companion/android/app/src/test/java/com/cacophony/companion/AgentsInventoryProvenanceTest.kt`
    (new, 10 tests).
- Tests: +10 unit tests; no existing tests changed.
- Behavioural delta: AgentsListScreen shows a one-line provenance pill
  immediately under the hero header. When the daemon serves fleet-latest
  the pill is green; when only local-only rows are available the pill
  turns amber. Older daemons that omit both fields are treated as
  fleet-latest so first paint never flashes a false client-local warning.

## Embedded artefacts

- None this session.

## Operator-takeaway

Android now matches the macOS companion: the agents pane surfaces explicit
"Source: daemon fleet-latest" or "Source: client-local" provenance driven
by the daemon's `inventory_mode` / `local_only` fields. The amber state is
the operator-visible signal that rows are not-yet-authoritative
client-local fallback (e.g. during a daemon restart-window). The
`AppStateStore.agentsInventory` flow is now available for any future
surfaces (NodesScreen, OverviewScreen) that want the same signal.
