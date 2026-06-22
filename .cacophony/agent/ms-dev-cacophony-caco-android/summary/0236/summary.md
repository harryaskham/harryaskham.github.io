# bd-296b38 WearOS terminal Shift-arrow helpers

## Goal
Add staged WearOS terminal quick-key helpers for common Shift-arrow modifier navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.ShiftArrowUp/Down/Right/Left` with xterm modifier sequences `\u001B[1;2A/B/C/D`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Shift-arrow labels.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
