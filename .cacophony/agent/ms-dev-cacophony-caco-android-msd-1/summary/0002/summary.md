# Session summary — Android pico system-prompt bubble (bd-d9de4d)

## Goal

Continue the Android pico chat UX parity work toward the macOS-pico bar. This
slice closes a concrete parity gap: the pico host emits `system_prompt` in the
AgentView snapshot and macOS renders it as a collapsible disclosure (bd-af11f9),
but Android's snapshot dropped the field entirely, so the agent's system prompt
was invisible on Android. Pure parse + UI — no session-source/transport change
(coordinated with md2-0, who owns the send-failure + slash-autocomplete contract
on bd-257184; this slice does not overlap).

## Bead(s)

- `bd-d9de4d` — Android pico: parse + render collapsible system-prompt bubble
  (macOS parity). Filed + claimed + implemented + validated this session.
- Context: sibling of the just-landed bd-76788d (composer) and bd-916717
  (empty state). Mirrors macOS bd-af11f9. Same "Rust emits a field Android
  drops" pattern md2-0 flagged for `available_commands`.

## Before state

- Failing tests: none.
- `PicoAgentViewSnapshot` had no `systemPrompt` field; `fromJson` never parsed
  `system_prompt`; the view never rendered it. macOS shows a collapsible
  "System prompt" bubble for the same snapshot.

## After state

- Failing tests: none. `:app:testDebugUnitTest --tests *.PicoAgentViewSourceTest`
  BUILD SUCCESSFUL, including the new `picoSystemPromptParsedAndRenderedBd_d9de4d`
  unit test (parses system_prompt; absent -> null; non-empty when only a system
  prompt is present).
- `PicoAgentViewSnapshot` now carries `systemPrompt: String?` parsed from
  `system_prompt`; `PicoSystemPromptBubble` renders a calm collapsed-by-default
  card at the top of the transcript; `picoSnapshotIsEmpty` treats a present
  system prompt as non-empty so it shows instead of "No messages yet".

## Diff summary

- Code/content commits: one commit (bd-d9de4d); final landed squash SHA from the
  reintegration receipt.
- Files touched:
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/pico/PicoAgentView.kt`
  - `companion/android/app/src/test/java/com/cacophony/companion/PicoAgentViewSourceTest.kt`
- Tests: +1 / -0 / flipped 0.
- Behavioural delta: system prompt now visible (collapsible) on Android pico;
  no transport/snapshot-reduction behavior changed.

## Validation note

Pure parse+UI validated by clean Kotlin compile + the PicoAgentViewSourceTest
lane (real fromJson + picoSnapshotIsEmpty assertions + a bubble source-pin).
Emulator capture deferred (live session needed; host recovering from an 8h load
storm); the proposed Compose-preview harness (draft bd-af78c3) would add visual
coverage.

## Operator-takeaway

Three pico-parity slices landed/queued this session (composer, empty state,
system-prompt bubble) plus a clean ownership split with md2-0, who owns the
transport-coupled gaps (send-failure surfacing, slash-command autocomplete) via
a shared PicoSessionSource contract. Next for me: transcript auto-scroll-to-bottom
(pure UI, non-overlapping), then wire the autocomplete + send-failure UI once
md2-0's availableCommands + lastSendFailure fields land.
