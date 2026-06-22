# Session summary — bd-0c1fc1 WatchAgentDetailScreen 'Open chat' chip

## Goal

Wear parity with bd-5d55e5 (phone AgentDetailScreen Open chat).
Operators on the watch viewing an agent detail get a Chat chip that
scopes the watch's existing WatchChatScreen to that agent's project.
Watch chat takes only `projectName: String?` so this is project-
scoped only (matches the bd-fdd6a2 BeadDetailScreen pattern on the
phone — per-agent narrowing is a phone-only refinement for now).

## Bead(s)

- `bd-0c1fc1` — WatchAgentDetailScreen — 'Open chat' chip
  (project-scoped jump, mirrors bd-5d55e5).

## After state

- `WatchAgentDetailScreen` accepts a new optional
  `onOpenChat: ((project: String) -> Unit)? = null` parameter.
- New Chat chip rendered between the Diff and Log chips, gated on
  `onOpenChat != null && d.project.isNotBlank()`. Uses the same
  `AdFrost.copy(alpha = 0.18f)` chip styling + 12sp label as Diff/
  Log/Files chips for visual consistency.
- `companion/android/wearable/src/main/java/com/cacophony/companion/wear/MainActivity.kt`
  wires `onOpenChat = { project -> scopedProject = project; nav.navigate(WatchDestination.Chat) }`.
- New `WatchAgentDetailOpenChatSourceTest` (3 tests) pins the
  optional param, chip gating + label + onClick, and MainActivity's
  scopedProject + navigate wiring.

## Diff summary

- Files touched (3):
  - `companion/android/wearable/src/main/java/com/cacophony/companion/wear/agents/WatchAgentDetailScreen.kt`
  - `companion/android/wearable/src/main/java/com/cacophony/companion/wear/MainActivity.kt`
  - `companion/android/wearable/src/test/java/com/cacophony/companion/wear/WatchAgentDetailOpenChatSourceTest.kt`
    (new, 3 tests).
- Tests: +3 source-pin tests; no existing tests changed.

## Operator-takeaway

On the watch, drill into any agent's detail and tap the new Chat chip
(below Diff, above Log) to jump straight to the watch chat scoped to
that agent's project.
