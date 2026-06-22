# Session summary — Android pico chat UX parity (composer + empty state)

## Goal

Drive the Android pico/chat surface toward the macOS-pico bar (simple, beautiful,
calm Material 3) per Harry's android pico-overhaul directive. Two focused,
non-colliding pure-UI slices this cycle: the composer affordance, and a calm
empty state for a connected session with no messages. Per Harry's fleet
directive (implement now, stagger the land while the merge queue drains), both
are committed on the agent branch to land together when the canonical-main-drift
infra blocker clears.

## Bead(s)

- `bd-76788d` — PicoAgentView composer: accent icon send button + contextual
  placeholder (macOS parity).
- `bd-916717` — PicoAgentView: calm "No messages yet" empty state for a connected
  session with no messages (macOS parity), instead of a blank LazyColumn.
- Context: complements md2-0's transport/FFI parity (`bd-257184`) and md2-1's
  landed header/states/thinking work; both slices are composer/empty-state UI
  only, no transport changes.

## Before state

- Failing tests: none.
- Composer used a static "Prompt pico agent" label + a plain text "Send" button.
- A connected session with an empty transcript rendered an empty LazyColumn — a
  blank gap between header and composer (the "avoid blank dashboards" anti-pattern;
  macOS shows "No messages yet").

## After state

- Failing tests: none. `:app:testDebugUnitTest --tests *.PicoAgentViewSourceTest`
  BUILD SUCCESSFUL twice (composer slice, then empty-state slice), including a new
  real unit test `picoSnapshotIsEmptyDetectsConnectedEmptySessionBd_916717` and a
  source-pin for the composer.
- Composer: contextual placeholder ("Message the agent…" / "Connect to a live
  pico agent to chat…") + Material 3 `FilledIconButton` paper-plane send.
- Empty state: `picoSnapshotIsEmpty` predicate + `PicoEmptyTranscriptBody` calm
  centered "No messages yet" body, wired into the PicoAgentView when-branch.

## Diff summary

- Code/content commits: two commits on the agent branch (bd-76788d, bd-916717);
  final landed squash SHA(s) come from the reintegration receipt.
- Files touched:
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/pico/PicoAgentView.kt`
  - `companion/android/app/src/test/java/com/cacophony/companion/PicoAgentViewSourceTest.kt`
- Tests: +2 / -0 / flipped 0 (one real predicate unit test, one composer source-pin).
- Behavioural delta: composer is icon-send + contextual placeholder; connected
  empty session shows a calm empty state. No transport/snapshot behavior changed.

## Validation note

Both slices validated by clean Kotlin compile + the PicoAgentViewSourceTest lane
(including a genuine `picoSnapshotIsEmpty` unit test). Emulator capture deferred
because the pico view renders meaningfully only with a live session and the host
is under post-recovery load; the proposed Compose-preview harness (draft
bd-af78c3) would close that visual-coverage gap.

## Operator-takeaway

The Android pico composer and empty-transcript states now match the macOS pico
shape. Landing is gated by the fleet-wide canonical-main-drift / merge-queue
infra blocker (bd-51993f), not by these changes — both slices are committed and
will land together the moment the queue drains. Next macOS-parity gaps are
send-failure surfacing and slash-command autocomplete, which need session-source
wiring and should be coordinated with md2-0 (bd-257184) to avoid collision.
