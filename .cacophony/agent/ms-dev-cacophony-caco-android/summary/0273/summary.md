# bd-3949af WearOS terminal Ctrl-Alt-Shift-F5-F8 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Alt-Shift-F5 through Ctrl-Alt-Shift-F8 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlAltShiftF5` through `CtrlAltShiftF8` with xterm modifier sequences `\u001B[15;8~`, `\u001B[17;8~`, `\u001B[18;8~`, and `\u001B[19;8~`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Alt-Shift-F5-F8.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
