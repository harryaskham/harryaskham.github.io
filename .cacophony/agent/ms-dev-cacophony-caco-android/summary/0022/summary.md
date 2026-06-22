# Session summary — bd-5d55e5 AgentDetailScreen → Chat jump

## Goal

Operators viewing an agent's detail page want a one-tap path into the
chat scoped to that agent. Previously they had to back out to the
Chat tab and manually select the project + agent in the sidebar.

Add an "Open chat" ActionButton in the AgentDetailScreen actions row
that takes them to the Chat tab with the chat pre-scoped to that
agent's project + agent channel.

## Bead(s)

- `bd-5d55e5` — AgentDetailScreen — 'Open chat with agent' button
  (jumps to chat scoped to this agent).

## After state

- `AgentDetailScreen` accepts a new optional
  `onOpenChat: ((project: String, agentId: String) -> Unit)? = null`
  parameter, threaded through `InfoTab` and `AgentActionsRow`.
- The chat-jump dispatch in InfoTab uses
  `onOpenChat?.takeIf { agent.project.isNotBlank() }?.let { hostOpen ->
  { hostOpen(agent.project, agent.id) } }` so the button only
  appears when both a host callback is provided AND the agent has a
  project to scope by.
- `AgentActionsRow` renders a new "Chat" `ActionButton` with
  `Icons.AutoMirrored.Filled.Chat` + `AuroraOrange` accent
  immediately after the Terminal button — keeping the two
  contextual surface-switch affordances visually adjacent.
- `ChatScreen` declares a new
  `CACO_CHAT_SCOPE_AGENT_PREF_KEY = "chat_scope_agent"` chatPrefs
  key for the one-shot agent selection. `selectedAgentChannel` is
  initialized by reading the pref and immediately clearing it so the
  next ordinary chat entry does NOT get stuck on the previous agent.
- `MainActivity` wires the AgentDetailScreen `onOpenChat`: writes
  project + agent prefs in one batch, clears `selectedAgentId`, flips
  `selectedTab = Tab.Chat`. ChatScreen first composition consumes
  both prefs and the operator lands directly in the scoped chat.
- New `AgentDetailOpenChatSourceTest` (5 tests) pins the
  AgentDetailScreen optional parameter, AgentActionsRow Chat button
  gating, InfoTab dispatch + blank-project safety, ChatScreen
  one-shot pref consume + clear, and MainActivity tab+prefs wire-up.

## Diff summary

- Files touched (4):
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/agents/AgentDetailScreen.kt`
    (param thread + Chat button render).
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/chat/ChatScreen.kt`
    (new pref key + one-shot initializer).
  - `companion/android/app/src/main/java/com/cacophony/companion/MainActivity.kt`
    (wire onOpenChat: write prefs + flip tab).
  - `companion/android/app/src/test/java/com/cacophony/companion/AgentDetailOpenChatSourceTest.kt`
    (new, 5 tests).
- Tests: +5 source-pin tests; no existing tests changed.

## Operator-takeaway

From any agent's detail page, tap the new "Chat" button to jump
straight to that agent's chat channel. One-shot — the next ordinary
Chat tab entry doesn't stick on this agent.
