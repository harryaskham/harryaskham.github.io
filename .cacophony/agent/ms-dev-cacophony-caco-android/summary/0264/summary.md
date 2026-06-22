# bd-d5e987 WearOS terminal Shift-F5-F8 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Shift-F5 through Shift-F8 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.ShiftF5` through `ShiftF8` with xterm modifier sequences `\u001B[15;2~`, `\u001B[17;2~`, `\u001B[18;2~`, and `\u001B[19;2~`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Shift-F5-F8.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
