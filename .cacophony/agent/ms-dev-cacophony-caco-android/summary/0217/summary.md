# bd-08e5c5 WearOS terminal Shift-Tab helper

## Goal
Add a staged WearOS terminal quick-key helper for the standard reverse-tab/backtab sequence while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.ShiftTab("Shift-Tab", "\u001B[Z")`.
- Pinned the ESC[Z input frame and label in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include `Shift-Tab`.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
