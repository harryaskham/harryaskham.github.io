# bd-20da6e WearOS terminal Ctrl-Shift-F5-F8 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Shift-F5 through Ctrl-Shift-F8 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlShiftF5` through `CtrlShiftF8` with xterm modifier sequences `\u001B[15;6~`, `\u001B[17;6~`, `\u001B[18;6~`, and `\u001B[19;6~`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Shift-F5-F8.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
