# bd-413b73 WearOS terminal Alt-Shift-F9-F12 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Alt-Shift-F9 through Alt-Shift-F12 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.AltShiftF9` through `AltShiftF12` with xterm modifier sequences `\u001B[20;4~`, `\u001B[21;4~`, `\u001B[23;4~`, and `\u001B[24;4~`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Alt-Shift-F9-F12.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
