# Session summary — bd-bf9c02 chat sidebar mark-all-as-read

## Goal

Companion affordance to bd-6fe94c's per-scope last-seen badges: long-
press the Global row in the chat sidebar to mark every visible
ProjectGroup + AgentRow as seen in one chatPrefs write. An operator
returning to chat after a long absence can clear the entire unread
surface with one gesture instead of visiting every row.

## Bead(s)

- `bd-bf9c02` — Chat sidebar — long-press Global to mark all scopes
  as read (follow-up to bd-6fe94c).

## After state

- `ChatSidebar` accepts a new `onMarkAllRead: () -> Unit = {}` callback.
- `ChatSidebarRowView` accepts a new `onLongClick: (() -> Unit)? = null`
  callback. When null, the row uses plain `.clickable(onClick)`;
  when non-null, it switches to
  `.combinedClickable(onClick, onLongClick)`.
- In the items{} dispatch loop, only `ChatSidebarRow.Global` gets a
  non-null `onLongClick` (project/agent rows pass null) so a stray
  long-press on a single scope cannot mass-clear unread context
  unexpectedly.
- `ChatScreen` defines `markAllScopesSeen()` that builds an updated
  lastSeen map with `now` for `"global"`, every visible
  `"project:<name>"`, and every visible `"agent:<project>:<id>"`,
  then writes the JSON once. Wired to `ChatSidebar` via
  `onMarkAllRead = { markAllScopesSeen() }`.
- New `ChatSidebarMarkAllReadSourceTest` (4 tests) pins onMarkAllRead
  param, Global-only long-press gating, combinedClickable / clickable
  branching, and the ChatScreen helper + wire-up.

## Diff summary

- Files touched (3):
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/chat/ChatSidebar.kt`
    (combinedClickable import, new onMarkAllRead + onLongClick params,
    Global-only dispatch).
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/chat/ChatScreen.kt`
    (new markAllScopesSeen helper + ChatSidebar wire-up).
  - `companion/android/app/src/test/java/com/cacophony/companion/ChatSidebarMarkAllReadSourceTest.kt`
    (new, 4 tests).
- Tests: +4 source-pin tests; no existing tests changed.

## Operator-takeaway

Long-press the "Global" row at the top of the chat sidebar (rail on
tablet, drawer on phone) to instantly mark every project + agent
unread badge as seen. The lastSeen JSON is persisted across app
restarts. Single-tap still works as before (open the scope and mark
just that one seen).
