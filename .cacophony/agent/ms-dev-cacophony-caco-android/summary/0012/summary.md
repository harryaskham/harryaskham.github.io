# Session summary — bd-4ef7a2 chat sidebar composable (slice 3 of bd-b3bc91)

## Goal

Slice 3 of the Android chat redesign decomp. Render the `ChatSidebarRow`
data model (slice 2 = bd-1b7958) inside the `ChatResponsiveLayout`
sidebar slot (slice 1 = bd-626dbd) so the Global / Projects / Agents
hierarchy is finally visible and clickable on tablet/foldable/landscape.

Phone portrait keeps the existing single-column UX unchanged because
ChatResponsiveLayout renders content() unwrapped below 600dp.

## Bead(s)

- `bd-4ef7a2` — Android chat redesign — sidebar composable (slice 3,
  child of `bd-b3bc91`).

## Before state

- `ChatResponsiveLayout`'s sidebarSlot (bd-626dbd) reserved a 260dp
  rail tinted `surfaceContainer` but rendered an explicit empty
  placeholder marker.
- `ChatSidebarRow` + `computeChatSidebarRows` (bd-1b7958) existed but
  had no consumer.

## After state

- New `companion/android/app/src/main/java/com/cacophony/companion/ui/chat/ChatSidebar.kt`:
  internal `ChatSidebar(rows, onSelectGlobal, onSelectProject,
  onSelectAgent)` composable rendering a `LazyColumn` of compact
  32dp-high rows. Each row applies `CHAT_SIDEBAR_INDENT_STEP_DP *
  row.indent` (16.dp step) for hierarchy indentation, tints
  `secondaryContainer` when `row.selected` with onSecondaryContainer
  label color and SemiBold weight, and shows a primary-tinted unread
  pill on the right when `row.unread > 0` (capped at "99+").
- `ChatScreen.kt` derives `sidebarRows` via
  `computeChatSidebarRows(messages, projects, agents,
  localSelectedProject, selectedAgentChannel)` inside a `remember`
  keyed on all five inputs. The `ChatResponsiveLayout` sidebarContent
  now instantiates `ChatSidebar(...)` with three callbacks that
  update `localSelectedProject` + `selectedAgentChannel` AND persist
  the project pref via `chatPrefs.edit().apply { ... }.apply()` so
  sidebar selections survive recompositions / app restarts exactly
  the way the in-screen project selector does.
- New `ChatSidebarSourceTest` (6 tests) pins composable signature,
  sealed-variant dispatch, indent rule, selection styling, unread
  badge gating + 99+ cap, and ChatScreen's full wiring (callback
  bodies, removed empty placeholder marker, sidebar instantiation
  inside the slot).
- `ChatResponsiveLayoutSourceTest` (bd-626dbd pin) updated to assert
  the new contract: `ChatResponsiveLayout sidebarContent = { ChatSidebar(...)`
  instead of the removed empty-placeholder marker.

## Diff summary

- Code commit: pending final squash SHA from reintegration receipt.
- Files touched (4):
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/chat/ChatSidebar.kt` (new, ~140 lines).
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/chat/ChatScreen.kt` (sidebarContent wire-up, ~30 in-context lines).
  - `companion/android/app/src/test/java/com/cacophony/companion/ChatSidebarSourceTest.kt` (new, 6 tests).
  - `companion/android/app/src/test/java/com/cacophony/companion/ChatResponsiveLayoutSourceTest.kt` (updated pin).
- Tests: +6 new + 1 updated; no other existing tests touched.
- Behavioural delta: phone portrait users see no change. Tablet /
  foldable / landscape users now see a real Global / Projects / Agents
  navigation sidebar in the rail. Selecting Global clears project +
  agent filters. Selecting a project header scopes to that project.
  Selecting an agent row scopes to that project + agent channel. All
  three click handlers persist their state to the same chatPrefs
  store the in-screen project selector uses, so behavior is
  consistent regardless of which surface the operator interacts
  with.

## Embedded artefacts

- None this session. Screenshot evidence will land once the operator
  installs the next release on a real tablet / foldable.

## Operator-takeaway

Open the rebuilt app on a tablet or in landscape on the phone (>=600dp
short edge) and the chat now shows a real left rail with Global at
the top, each project below it, and each project's agents nested
beneath. Click anything to switch the chat scope. Phone portrait UX
is byte-identical to before. The decomp is now 3 of 5 slices done
(scaffold + data + composable); remaining children are web-parity
polish (`bd-db6c51` follow-up) and an optional phone-portrait drawer
that surfaces the same sidebar via swipe-from-left-edge gesture.
