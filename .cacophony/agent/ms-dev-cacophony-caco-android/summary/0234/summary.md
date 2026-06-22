# bd-8a2046 WearOS terminal Ctrl-arrow helpers

## Goal
Add staged WearOS terminal quick-key helpers for common Ctrl-arrow modifier navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlArrowUp/Down/Right/Left` with xterm modifier sequences `\u001B[1;5A/B/C/D`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-arrow labels.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
