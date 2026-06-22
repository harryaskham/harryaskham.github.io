# bd-552018 WearOS terminal Alt-arrow helpers

## Goal
Add staged WearOS terminal quick-key helpers for common Alt-arrow modifier navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.AltArrowUp/Down/Right/Left` with xterm modifier sequences `\u001B[1;3A/B/C/D`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Alt-arrow labels.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
