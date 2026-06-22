# bd-14bdf4 WearOS terminal Alt-F1-F4 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Alt-F1 through Alt-F4 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.AltF1` through `AltF4` with xterm modifier sequences `\u001B[1;3P/Q/R/S`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Alt-F1-F4.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
