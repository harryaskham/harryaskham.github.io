# bd-007f74 WearOS terminal Ctrl-Shift-Home/End helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Shift-Home/Ctrl-Shift-End navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlShiftHome("Ctrl-Shift-Home", "\u001B[1;6H")` and `CtrlShiftEnd("Ctrl-Shift-End", "\u001B[1;6F")`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Shift Home/End.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
