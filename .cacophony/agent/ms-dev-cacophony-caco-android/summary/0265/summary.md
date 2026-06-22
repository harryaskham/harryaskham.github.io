# bd-a99693 WearOS terminal Shift-F9-F12 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Shift-F9 through Shift-F12 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.ShiftF9` through `ShiftF12` with xterm modifier sequences `\u001B[20;2~`, `\u001B[21;2~`, `\u001B[23;2~`, and `\u001B[24;2~`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Shift-F9-F12.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
