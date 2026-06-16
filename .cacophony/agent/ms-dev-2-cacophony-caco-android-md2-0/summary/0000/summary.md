# Session summary — Android chat global broadcast scope dropdown (bd-580053)

## Goal

Give the Android companion chat composer an explicit Project-vs-Global broadcast
audience selector. Operators could previously only press "Broadcast", with no
way to choose whether the message went to one project's agents or fanned out
across all projects. While implementing this I also found and fixed a latent
routing bug: every Android "Broadcast" was already being delivered globally
despite the UI calling it a "project broadcast".

## Bead(s)

- `bd-580053` — Add global broadcast dropdown option to Android chat (P2 feature, android/chat/messaging)

(Also investigated `bd-bcc919` — Pico /session connection — before a sibling agent
claimed it; handed off my findings, no overlap.)

## Before state

- Failing tests: none.
- `ConnectionManager.broadcast(project, body)` unconditionally POSTed to the
  GLOBAL endpoint `/api/v1/messages/broadcast` with a `project` field that the
  daemon's `GlobalBroadcastRequest` ignores — so a "project broadcast" was
  silently delivered to every project's agents.
- Chat composer had only the send-mode chip (Direct / Broadcast / Speak); the
  Broadcast placeholder read "Write a project broadcast…" but no project scope
  was actually applied.
- Daemon already exposes both endpoints: project-scoped
  `/api/v1/projects/{project}/messages/broadcast` (BroadcastMessageRequest, has
  a `global` flag) and global `/api/v1/messages/broadcast` (GlobalBroadcastRequest).

## After state

- Failing tests: none. New focused JVM test `ChatBroadcastScopeSourceTest`
  passes (`gradle :app:testDebugUnitTest --tests ...` BUILD SUCCESSFUL); full
  `compileDebugKotlin` + `assembleDebug` succeed.
- Chat composer shows a second chip in Broadcast mode: a **Project / Global**
  scope dropdown (default Project). Placeholder updates to "Write a global
  broadcast…" when Global is chosen.
- `broadcast(project, body, global=false)` now routes by scope:
  - Project → `/api/v1/projects/{project}/messages/broadcast` with `global=false`.
  - Global  → `/api/v1/messages/broadcast` with `{body}` only.
- Verified on an x86_64/KVM emulator (medium_phone, headless): the Chat screen
  renders the new "Project" scope chip next to "Broadcast" with the project
  placeholder (see screenshot).

## Diff summary

- Code/content commits: final landed squash SHA from the reintegration receipt.
- Files touched:
  - `companion/android/app/src/main/java/com/cacophony/companion/connection/ConnectionManager.kt`
    — `broadcast()` gains a `global` flag; new pure `broadcastRequestSpec()` +
    `BroadcastRequestSpec` helper for endpoint/payload routing (unit-testable).
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/chat/ChatScreen.kt`
    — new `BroadcastScope` enum (Project/Global), composer scope-picker chip +
    dropdown shown in Broadcast mode, `broadcastScopeColor()` accent, scope-aware
    placeholder, send wiring passing `global = scope == Global`.
  - `companion/android/app/src/test/java/com/cacophony/companion/ChatBroadcastScopeSourceTest.kt`
    — new (behavioural routing assertions + source pins).
- Tests: +1 test class (5 tests).
- Behavioural delta: broadcasts now respect a chosen scope; project broadcasts
  are correctly project-scoped (previously always global).

## Embedded artefacts

- `screenshots/android-0000-chat-broadcast-scope.png` — Chat screen with the new
  Project scope chip + Broadcast chip and "Write a project broadcast…" placeholder.
- `screenshots/android-0000-chat-broadcast-scope-open.png` — secondary capture
  (dropdown-open attempt; a headless-emulator SystemUI ANR blocked the expanded
  menu capture — unrelated to the change).

## Operator-takeaway

The visible win is the Project/Global broadcast chooser, but the more important
fix is the silent routing bug underneath it: Android "broadcasts" were all going
global. This is exactly the cross-surface API-contract class Harry asked about
earlier — a client posting to the wrong endpoint with a field the server ignores,
with no loud failure. A daemon-side consumer-contract test (assert the project
broadcast endpoint is hit for project scope) would have caught it; worth adding
to the mobile API-contract suite.
