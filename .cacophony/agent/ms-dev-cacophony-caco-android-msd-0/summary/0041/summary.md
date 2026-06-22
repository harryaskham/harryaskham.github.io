# Session summary — bd-caabe3 slice 2: configured group chats in the chat sidebar

## Goal

Display configured Cacophony group chats in the Android chat surface as selectable
channels, per the SPEC group-chat IA (groups under their project, read-only).

## Bead(s)

- `bd-caabe3` — Support caco group chat in the Android app chat surface (claimed,
  in_progress). Slice 2 (display); slices 1 + 2a (read model + resolution) landed
  @29bbebbd78 / @a651efe131.

## After state

- `state/ChatSidebarRow.kt`: new `ChatSidebarRow.GroupRow` (project, groupId,
  members, displayLabel; key `group:<project>:<id>`, indent 1).
  `computeChatSidebarRows` gained a defaulted `selectedGroup` param and now builds
  GroupRows from `ProjectSnapshot.agentGroups` after each project's AgentRows in
  case-insensitive group-id order; ProjectGroup selection now also requires no group
  selected. Backward-compatible (projects without groups render identically).
- `ui/chat/ChatSidebar.kt`: GroupRow handled in the a11y content-description `when`
  ("Group <label> in <project>"), the search filter, the no-matches guard, and the
  render `when` (→ `onSelectGroup`); new `onSelectGroup` composable param.
- `ui/chat/ChatScreen.kt`: `selectedGroupChannel` state threaded into the builder
  (and its remember key), cleared when selecting global/project/agent, and set by a
  new `onSelectGroup` that scopes the chat to the group.
- Source-pin tests updated: ChatPhoneDrawerSourceTest regex (GroupRow is the last
  render branch before `sink.onSelection()`), ChatSidebarA11ySourceTest (+GroupRow
  case), ChatSidebarEmptySearchSourceTest (hasNoMatches +GroupRow). Added a
  computeChatSidebarRows GroupRow builder test (ChatSidebarRowsTest).

## Validation

- Queued (host-safe) FULL `gradle :app:testDebugUnitTest` (job tj-d036bed2) →
  passed (Compose + every source-pin). Full suite required because this touches
  ChatScreen/ChatSidebar Compose + source-pinned sidebar structure. No forbidden
  literals.

## Diff summary

- Code commit: bd-caabe3 slice 2; landed squash SHA from receipt.
- Files: `state/ChatSidebarRow.kt`, `ui/chat/ChatSidebar.kt`, `ui/chat/ChatScreen.kt`,
  4 chat test files.

## Operator-takeaway

Configured Cacophony group chats now appear in the Android chat sidebar under their
project and are selectable. Selecting a group scopes the chat to it; sending to the
group (per-member expansion) and the `via <group>` history marker are the next
slices.

## Remaining gaps

- bd-caabe3 slice 3 (group-scoped compose → per-member sends + per-target
  diagnostics), slice 4 (`via <group>` history marker).
