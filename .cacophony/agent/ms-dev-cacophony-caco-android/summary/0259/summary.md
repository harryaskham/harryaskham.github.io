# bd-2a984c WearOS terminal Ctrl-Alt-Shift Insert/Delete helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Alt-Shift-Insert/Ctrl-Alt-Shift-Delete edit keys while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlAltShiftInsert("Ctrl-Alt-Shift-Insert", "\u001B[2;8~")` and `CtrlAltShiftDelete("Ctrl-Alt-Shift-Delete", "\u001B[3;8~")`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Alt-Shift Insert/Delete.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
