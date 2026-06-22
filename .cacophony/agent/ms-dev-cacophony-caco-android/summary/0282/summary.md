# bd-8103f5 WearOS terminal Alt-Shift arrow helpers

## Goal
Add staged WearOS terminal quick-key helpers for Alt-Shift arrow navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.AltShiftArrowUp/Down/Right/Left` with xterm modifier sequences `\u001B[1;4A/B/C/D`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Alt-Shift arrows.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
