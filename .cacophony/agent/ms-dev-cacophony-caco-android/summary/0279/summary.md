# bd-21c4f5 WearOS terminal Alt-Shift-F5-F8 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Alt-Shift-F5 through Alt-Shift-F8 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.AltShiftF5` through `AltShiftF8` with xterm modifier sequences `\u001B[15;4~`, `\u001B[17;4~`, `\u001B[18;4~`, and `\u001B[19;4~`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Alt-Shift-F5-F8.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
