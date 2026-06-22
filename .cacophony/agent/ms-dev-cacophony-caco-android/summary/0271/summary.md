# bd-ff19ef WearOS terminal Ctrl-Alt-F9-F12 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Alt-F9 through Ctrl-Alt-F12 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlAltF9` through `CtrlAltF12` with xterm modifier sequences `\u001B[20;7~`, `\u001B[21;7~`, `\u001B[23;7~`, and `\u001B[24;7~`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Alt-F9-F12.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
