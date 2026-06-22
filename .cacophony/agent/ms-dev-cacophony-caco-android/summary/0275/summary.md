# bd-83041f WearOS terminal Ctrl-Shift-F1-F4 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Shift-F1 through Ctrl-Shift-F4 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlShiftF1` through `CtrlShiftF4` with xterm modifier sequences `\u001B[1;6P/Q/R/S`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Shift-F1-F4.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
