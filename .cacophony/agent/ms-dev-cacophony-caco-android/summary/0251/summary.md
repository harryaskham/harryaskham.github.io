# bd-fd09b1 WearOS terminal Alt-Shift-Home/End helpers

## Goal
Add staged WearOS terminal quick-key helpers for Alt-Shift-Home/Alt-Shift-End navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.AltShiftHome("Alt-Shift-Home", "\u001B[1;4H")` and `AltShiftEnd("Alt-Shift-End", "\u001B[1;4F")`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Alt-Shift Home/End.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
