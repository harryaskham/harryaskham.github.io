# bd-d91cd5 WearOS terminal Ctrl-Alt-Shift-Home/End helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Alt-Shift-Home/Ctrl-Alt-Shift-End navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlAltShiftHome("Ctrl-Alt-Shift-Home", "\u001B[1;8H")` and `CtrlAltShiftEnd("Ctrl-Alt-Shift-End", "\u001B[1;8F")`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Alt-Shift Home/End.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
