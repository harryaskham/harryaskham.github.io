# Session summary — WearOS PTY input-disabled reason copy

## Goal

Add compact model-level disabled-reason copy for future WearOS terminal keyboard/dictation controls when input cannot be sent.

## Bead(s)

- `bd-e427bd` — WearOS PTY state exposes input-disabled reason copy

## Before state

- Failing tests: none before this slice.
- Relevant metrics: `watchPtyCanSendInput` provided a boolean, but future UI would still need to duplicate state-specific explanation strings for read-only, connecting, error, closed, and idle states.
- Context: focused child of broad WearOS live terminal parent `bd-7b4a80`; pure model helper only.

## After state

- Failing tests: none in final focused validation.
- Relevant metrics: added `watchPtyInputDisabledReason(state)`, returning null for live writable state, `Terminal is read-only` for live-readOnly/ReadOnly, and compact distinct copy for Connecting, Error, Closed, and Idle.
- Context: no WebSocket, UI, or RemoteInput changes.

## Diff summary

- Code/content commits: `fc8c86c6be`; final landed squash SHA comes from the reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `companion/android/wearable/src/main/java/com/cacophony/companion/wear/terminal/WatchPtyState.kt`, `companion/android/wearable/src/test/java/com/cacophony/companion/wear/WatchPtyStateSourceTest.kt`.
- Tests: `tj-e3ec2088` passed (`:wearable:testDebugUnitTest --tests com.cacophony.companion.wear.WatchPtyStateSourceTest`); `bj-b31f6138` succeeded (`:wearable:assembleRelease`).

## Operator-takeaway

WearOS PTY state now has both a safe can-send predicate and compact disabled-reason copy for future terminal input controls.
