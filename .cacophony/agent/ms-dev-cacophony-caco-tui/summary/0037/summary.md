# Session summary — WearOS PTY input writability helper

## Goal

Add a pure WearOS PTY state helper for future terminal input controls so keyboard/dictation UI can gate sends consistently from reducer state.

## Bead(s)

- `bd-b26d76` — WearOS PTY state exposes input writability helper

## Before state

- Failing tests: none before this slice.
- Relevant metrics: future terminal UI callers would need to duplicate `connectionState` and `readOnly` checks before sending input, risking sends while idle, connecting, read-only, errored, or closed.
- Context: focused child of broad WearOS live terminal parent `bd-7b4a80`; pure model helper only.

## After state

- Failing tests: none in final focused validation.
- Relevant metrics: added `watchPtyCanSendInput(state)`, which returns true only when `connectionState == Live` and `readOnly == false`; Idle, Connecting, ReadOnly, Error, Closed, and live-readOnly all return false.
- Context: no WebSocket, UI, or RemoteInput changes.

## Diff summary

- Code/content commits: `2220d830d1`; final landed squash SHA comes from the reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `companion/android/wearable/src/main/java/com/cacophony/companion/wear/terminal/WatchPtyState.kt`, `companion/android/wearable/src/test/java/com/cacophony/companion/wear/WatchPtyStateSourceTest.kt`.
- Tests: `tj-156d2848` passed (`:wearable:testDebugUnitTest --tests com.cacophony.companion.wear.WatchPtyStateSourceTest`); `bj-60ca2c6b` succeeded (`:wearable:assembleRelease`).

## Operator-takeaway

WearOS PTY state now has a single safe predicate for future terminal input affordances, preventing accidental sends outside live writable sessions.
