# bd-fde27a WearOS terminal Ctrl-Alt-F1-F4 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Alt-F1 through Ctrl-Alt-F4 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlAltF1` through `CtrlAltF4` with xterm modifier sequences `\u001B[1;7P/Q/R/S`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Alt-F1-F4.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
