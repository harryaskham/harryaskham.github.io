# bd-de7d58 WearOS terminal Shift-Insert/Shift-Delete helpers

## Goal
Add staged WearOS terminal quick-key helpers for Shift-Insert/Shift-Delete edit keys while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.ShiftInsert("Shift-Insert", "\u001B[2;2~")` and `ShiftDelete("Shift-Delete", "\u001B[3;2~")`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Shift-Insert/Delete.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
