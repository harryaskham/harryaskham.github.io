# bd-77eb91 WearOS terminal Ctrl-Alt-Shift-F9-F12 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Alt-Shift-F9 through Ctrl-Alt-Shift-F12 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlAltShiftF9` through `CtrlAltShiftF12` with xterm modifier sequences `\u001B[20;8~`, `\u001B[21;8~`, `\u001B[23;8~`, and `\u001B[24;8~`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Alt-Shift-F9-F12.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
