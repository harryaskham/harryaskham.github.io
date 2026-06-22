# bd-7988f8 WearOS terminal Ctrl-Shift-F9-F12 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Shift-F9 through Ctrl-Shift-F12 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlShiftF9` through `CtrlShiftF12` with xterm modifier sequences `\u001B[20;6~`, `\u001B[21;6~`, `\u001B[23;6~`, and `\u001B[24;6~`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Shift-F9-F12.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
