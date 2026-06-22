# bd-3221d0 WearOS terminal F9-F12 helpers

## Goal
Add staged WearOS terminal quick-key helpers for xterm F9-F12 function keys while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.F9` through `F12` with CSI sequences `\u001B[20~`, `\u001B[21~`, `\u001B[23~`, and `\u001B[24~`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include F9-F12.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
