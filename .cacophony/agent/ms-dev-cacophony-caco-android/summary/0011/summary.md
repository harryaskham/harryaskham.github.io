# Session summary — bd-1b7958 chat sidebar data model (slice 2 of bd-b3bc91)

## Goal

Slice 2 of the Android chat redesign decomp (slice 1 = bd-626dbd
responsive layout scaffold, landed). Provide the data model the
upcoming sidebar composable will render: a flat list of
`ChatSidebarRow` entries describing the Global / Projects / Agents
hierarchy with selection state and unread badges derived from
existing `AppStateStore` flows.

Pure data layer; no UI, no Rust delta. ChatResponsiveLayout's
sidebarSlot continues to render empty until a follow-up child wires
the sidebar composable.

## Bead(s)

- `bd-1b7958` — Android chat redesign — sidebar data model (slice 2,
  child of `bd-b3bc91`).
- Closed earlier this loop tick: `bd-bb446c` (Wearable Home sectioned
  nav) as superseded by bd-ca56b5 / slice 203 (Home already grouped
  into 12 HomeGroup chips; the 65-row flat-list premise is stale).

## Before state

- `bd-626dbd` ChatResponsiveLayout reserved a sidebar slot but had
  nothing to put in it.
- No sidebar data model existed; the chat hierarchy structure
  (Global / Projects / Agents) lived implicitly in ChatScreen's
  project-filter + agent-channel state.

## After state

- New `companion/android/app/src/main/java/com/cacophony/companion/state/ChatSidebarRow.kt`:
  sealed `ChatSidebarRow` with `Global`, `ProjectGroup`, `AgentRow`
  variants plus stable `key` / `label` / `indent` / `unread` /
  `selected` interface. Layout invariant: Global at index 0, then
  alphabetically-sorted ProjectGroups with their AgentRows nested
  alphabetically by displayLabel.
- New pure `computeChatSidebarRows(messages, projects, agents,
  selectedProject, selectedAgent)`: derives the flat row list from
  AppStateStore inputs. Unread aggregation rolls up at three levels
  (Global = all messages, ProjectGroup = per-project, AgentRow = per
  agent matched via sender OR target). Selection propagates per
  `selectedProject==null -> Global`, `selectedProject + blank agent
  -> ProjectGroup`, `selectedProject + selectedAgent -> AgentRow`.
- New `ChatSidebarRowsTest` (6 unit tests) covering empty inputs,
  single-project alphabetical ordering, multi-project ordering with
  nested agents, three-level unread aggregation including unmapped-
  project + blank-project messages, three-tier selection propagation
  (Global / ProjectGroup / AgentRow each in turn), and defensive
  filtering of blank project/agent rows.

## Diff summary

- Code commit: pending final squash SHA from reintegration receipt.
- Files touched (2):
  - `companion/android/app/src/main/java/com/cacophony/companion/state/ChatSidebarRow.kt`
    (new, ~165 lines).
  - `companion/android/app/src/test/java/com/cacophony/companion/ChatSidebarRowsTest.kt`
    (new, 6 tests).
- Tests: +6 unit tests; no existing tests changed.
- Behavioural delta: none visible to operators yet — pure data layer.
  The next slice (sidebar composable) consumes this model to render
  the Global / Projects / Agents tree inside ChatResponsiveLayout's
  sidebar slot.

## Embedded artefacts

- None this session.

## Operator-takeaway

Chat redesign now has both the responsive layout slot (bd-626dbd)
AND the data model behind the sidebar (bd-1b7958). The next slice
in the decomp — sidebar composable consuming this model into the
slot — is a small Compose change that finally renders the hierarchy.
