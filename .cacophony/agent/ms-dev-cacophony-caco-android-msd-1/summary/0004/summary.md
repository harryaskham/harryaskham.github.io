# Session summary — Android pico send-failure banner (bd-4c2f6c)

## Goal

Continue Android pico chat UX parity with macOS. This slice surfaces transient
send failures: when a sendPrompt fails, the composer shows a calm error-tinted
banner with the reason and a Retry that re-sends the failed text, distinct from
the session going Failed — matching the macOS lastSendFailed banner. UI half of
the md2-0 split (md2-0 plumbs source/snapshot state; I build UI), reading the
already-landed source.lastSendFailure.

## Bead(s)

- `bd-4c2f6c` — Android pico composer: send-failure banner with retry from
  source.lastSendFailure (macOS parity). Filed + claimed + implemented +
  validated this session.
- Builds on md2-0's lastSendFailure plumbing (bd-257184, on main). Sibling of
  landed bd-76788d/bd-916717/bd-d9de4d/bd-41adfa.

## Before state

- Failing tests: none in the pico lane.
- A failed sendPrompt was silent in the UI even though
  source.lastSendFailure (PicoSendFailure text/reason/atMillis) was populated.

## After state

- Failing tests: none in the pico lane. `:app:testDebugUnitTest` on
  PicoAgentViewSourceTest + PicoStandaloneActivitySourceTest BUILD SUCCESSFUL
  (1m51s), including the new `picoSendFailureLabelAndBannerBd_4c2f6c` test.
- PicoStandaloneActivity polls source.lastSendFailure (alongside snapshot/state)
  and passes it to PicoAgentView; `PicoSendFailureBanner` renders above the
  composer with the reason + a Retry that re-sends; a successful send clears the
  source state and the banner. Pure `picoSendFailureLabel` formats the message.

## Diff summary

- Code/content commits: one commit (bd-4c2f6c); final landed squash SHA from the
  reintegration receipt.
- Files touched:
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/pico/PicoAgentView.kt`
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/pico/PicoStandaloneActivity.kt`
  - `companion/android/app/src/test/java/com/cacophony/companion/PicoAgentViewSourceTest.kt`
- Tests: +1 / -0 / flipped 0.
- Behavioural delta: failed sends are now visible + retryable; no source
  interface change (UI + activity polling only).

## Validation note

UI + activity-polling only, validated by clean Kotlin compile + both pico source
test lanes. Emulator capture deferred (live session + an induced send failure
needed; host recovering). Compose-preview harness (draft bd-af78c3) would add
visual coverage.

## Operator-takeaway

Six pico-parity slices this session (single-builder rule, composer, empty state,
system-prompt bubble, slash-command autocomplete, send-failure banner). The
data/UI split with md2-0 keeps landing conflict-free. Next queued UI: the model
picker (availableModels + pendingModelPicker landed at 80d03c3d5d), then
pending_dialog once md2-0 resumes field plumbing after its P1 (bd-618fc6).
