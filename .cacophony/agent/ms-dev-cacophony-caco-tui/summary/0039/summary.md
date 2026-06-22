# Session summary — WearOS terminal shell input status row

## Goal

Surface staged input-disabled reason copy in the WearOS terminal shell so future keyboard/dictation controls have an operator-visible explanation before live input wiring lands.

## Bead(s)

- `bd-7c3405` — WearOS terminal shell shows staged input disabled reason

## Before state

- Failing tests: none before this slice.
- Relevant metrics: terminal shell staged state/output/quick-key copy but did not display why input was disabled in the non-live preview state.
- Context: focused child of broad WearOS live terminal parent `bd-7b4a80`; shell remains non-live.

## After state

- Failing tests: none in final focused validation.
- Relevant metrics: added `watchTerminalShellInputStatus`, delegating to `watchPtyInputDisabledReason` and falling back to `Input ready`; staged shell now renders `Input: Terminal not connected` for idle preview state.
- Context: no WebSocket, input send path, keyboard, or RemoteInput implementation was added.

## Diff summary

- Code/content commits: `d1b51dd812`; final landed squash SHA comes from the reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `companion/android/wearable/src/main/java/com/cacophony/companion/wear/terminal/WatchTerminalShellScreen.kt`, `companion/android/wearable/src/test/java/com/cacophony/companion/wear/WatchTerminalShellSourceTest.kt`.
- Tests: `tj-1b2cf9bc` passed (`:wearable:testDebugUnitTest --tests com.cacophony.companion.wear.WatchTerminalShellSourceTest`); `bj-c58bd530` succeeded (`:wearable:assembleRelease`).

## Operator-takeaway

The staged WearOS terminal shell now explains disabled input state without changing live-terminal runtime behavior.
