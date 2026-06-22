# bd-0ff96c WearOS terminal Alt-F5-F8 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Alt-F5 through Alt-F8 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.AltF5` through `AltF8` with xterm modifier sequences `\u001B[15;3~`, `\u001B[17;3~`, `\u001B[18;3~`, and `\u001B[19;3~`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Alt-F5-F8.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
