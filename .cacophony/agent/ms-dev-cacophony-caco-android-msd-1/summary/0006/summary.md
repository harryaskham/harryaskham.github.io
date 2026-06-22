# Session summary — Android pico per-message timestamps (bd-9296b7)

## Goal

Continue Android pico chat UX parity with macOS by showing per-message
timestamps in the transcript, reading md2-0's just-landed
snapshot.transcriptTimestamps (index-aligned unix-ms). UI half of the md2-0
data/UI split.

## Bead(s)

- `bd-9296b7` — Android pico transcript: render per-message timestamps from
  snapshot.transcriptTimestamps (macOS parity). Filed + claimed + implemented +
  validated this session.
- Builds on md2-0's transcript_ts/notifications plumbing (bd-be3305, 24f44631f0).

## Before state

- Failing tests: none in the pico lane.
- Transcript bubbles showed sender + body but no time, despite
  snapshot.transcriptTimestamps now being populated.

## After state

- Failing tests: none. `:app:testDebugUnitTest --tests *.PicoAgentViewSourceTest`
  BUILD SUCCESSFUL (3m10s), including the new `picoFormatTimestampBd_9296b7` test.
- Each committed transcript bubble shows a subtle HH:mm time after the sender
  label; streaming/status rows have none; non-positive millis render nothing.
  Pure `picoFormatTimestamp(millis, zone)` formats it.

## Diff summary

- Code/content commits: one commit (bd-9296b7); landed squash SHA from receipt.
- Files touched:
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/pico/PicoAgentView.kt`
  - `companion/android/app/src/test/java/com/cacophony/companion/PicoAgentViewSourceTest.kt`
- Tests: +1 / -0 / flipped 0.
- Behavioural delta: transcript messages show their time; no transport change.

## Validation note

Pure formatter unit-tested (fixed UTC zone for determinism); bubble wiring
source-pinned + compiles clean. Emulator capture deferred (live session needed).

## Operator-takeaway

Eight slices this session toward a seamless pico agentic experience. md2-0 fully
traced the model-picker blocker: it will add selectModel(index): Boolean to the
PicoSessionSource interface (one send-command bridge that also unblocks
pending_dialog reply), then ping me. Next buildable slice while that's pending:
the notifications UI (snapshot.notifications also landed at 24f44631f0).
