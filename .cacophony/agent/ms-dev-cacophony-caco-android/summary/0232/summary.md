# bd-fc6a56 WearOS terminal F5-F8 helpers

## Goal
Add staged WearOS terminal quick-key helpers for xterm F5-F8 function keys while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.F5` through `F8` with CSI sequences `\u001B[15~`, `\u001B[17~`, `\u001B[18~`, and `\u001B[19~`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include F5-F8.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
