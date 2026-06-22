# bd-f787a4 WearOS terminal Alt-Shift Page helpers

## Goal
Add staged WearOS terminal quick-key helpers for Alt-Shift-PageUp/PageDown navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.AltShiftPageUp("Alt-Shift-PageUp", "\u001B[5;4~")` and `AltShiftPageDown("Alt-Shift-PageDown", "\u001B[6;4~")`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Alt-Shift PageUp/Down.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
