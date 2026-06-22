# bd-161b3f WearOS terminal Ctrl-Shift Insert/Delete helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Shift-Insert/Ctrl-Shift-Delete edit keys while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlShiftInsert("Ctrl-Shift-Insert", "\u001B[2;6~")` and `CtrlShiftDelete("Ctrl-Shift-Delete", "\u001B[3;6~")`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Shift Insert/Delete.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
