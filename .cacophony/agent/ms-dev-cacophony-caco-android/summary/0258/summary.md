# bd-ca60d9 WearOS terminal Ctrl-Alt-Shift Page helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Alt-Shift-PageUp/PageDown navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlAltShiftPageUp("Ctrl-Alt-Shift-PageUp", "\u001B[5;8~")` and `CtrlAltShiftPageDown("Ctrl-Alt-Shift-PageDown", "\u001B[6;8~")`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Alt-Shift PageUp/Down.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
