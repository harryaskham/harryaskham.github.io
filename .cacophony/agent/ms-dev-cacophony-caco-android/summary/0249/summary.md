# bd-556f10 WearOS terminal Alt-Shift Insert/Delete helpers

## Goal
Add staged WearOS terminal quick-key helpers for Alt-Shift-Insert/Alt-Shift-Delete edit keys while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.AltShiftInsert("Alt-Shift-Insert", "\u001B[2;4~")` and `AltShiftDelete("Alt-Shift-Delete", "\u001B[3;4~")`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Alt-Shift Insert/Delete.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
