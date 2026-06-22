# bd-d1c5cb WearOS terminal Shift-Page helpers

## Goal
Add staged WearOS terminal quick-key helpers for Shift-PageUp/PageDown navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.ShiftPageUp("Shift-PageUp", "\u001B[5;2~")` and `ShiftPageDown("Shift-PageDown", "\u001B[6;2~")`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Shift-PageUp/Down.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
