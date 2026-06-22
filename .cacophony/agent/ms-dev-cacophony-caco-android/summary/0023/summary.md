# Session summary — bd-fdd6a2 BeadDetailScreen → Project chat jump

## Goal

Sibling of bd-5d55e5 (AgentDetailScreen Open chat). Operators viewing
a bead want a one-tap path into the chat scoped to the bead's project
(no per-agent narrowing — beads belong to projects, not agents).

## Bead(s)

- `bd-fdd6a2` — BeadDetailScreen — 'Open project chat' button.

## After state

- `BeadDetailScreen` accepts a new
  `onOpenProjectChat: ((project: String) -> Unit)? = null` param.
- New OutlinedButton labeled "Project chat" with
  `Icons.AutoMirrored.Filled.Chat` + `AuroraOrange` rendered after the
  Close button. Gated on `onOpenProjectChat != null AND liveBead.project
  .isNotBlank()`.
- `MainActivity` wires the jump: writes
  `CACO_CHAT_SCOPE_PROJECT_PREF_KEY`, clears the one-shot
  `CACO_CHAT_SCOPE_AGENT_PREF_KEY` (so a previous agent selection
  doesn't leak into the project-wide view), clears `selectedBead`,
  flips `selectedTab = Tab.Chat`.
- New `BeadDetailOpenProjectChatSourceTest` (3 tests) pins the
  optional param, button gating + label + onClick wiring, and
  MainActivity's tab+prefs wire-up including the agent-key clear.

## Diff summary

- Files touched (3):
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/beads/BeadDetailScreen.kt`
  - `companion/android/app/src/main/java/com/cacophony/companion/MainActivity.kt`
  - `companion/android/app/src/test/java/com/cacophony/companion/BeadDetailOpenProjectChatSourceTest.kt`
    (new, 3 tests).
- Tests: +3 source-pin tests.

## Operator-takeaway

From any bead's detail page, tap "Project chat" to jump straight to
the bead's project chat (no agent filter). One-shot — the next chat
entry doesn't stick on this project unless re-selected via sidebar.
