# bd-ca86bb WearOS terminal Ctrl-F9-F12 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-F9 through Ctrl-F12 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlF9` through `CtrlF12` with xterm modifier sequences `\u001B[20;5~`, `\u001B[21;5~`, `\u001B[23;5~`, and `\u001B[24;5~`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-F9-F12.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
