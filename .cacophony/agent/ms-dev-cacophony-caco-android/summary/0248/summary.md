# bd-e37708 WearOS terminal Ctrl-Shift-Page helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Shift-PageUp/PageDown navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlShiftPageUp("Ctrl-Shift-PageUp", "\u001B[5;6~")` and `CtrlShiftPageDown("Ctrl-Shift-PageDown", "\u001B[6;6~")`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Shift PageUp/Down.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
