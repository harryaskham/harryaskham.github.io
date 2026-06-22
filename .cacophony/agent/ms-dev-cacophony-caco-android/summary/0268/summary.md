# bd-5566ea WearOS terminal Alt-F9-F12 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Alt-F9 through Alt-F12 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.AltF9` through `AltF12` with xterm modifier sequences `\u001B[20;3~`, `\u001B[21;3~`, `\u001B[23;3~`, and `\u001B[24;3~`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Alt-F9-F12.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
