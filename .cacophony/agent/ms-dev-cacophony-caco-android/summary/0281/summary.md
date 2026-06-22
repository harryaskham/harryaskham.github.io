# bd-f8f7e0 WearOS terminal Ctrl-Shift arrow helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Shift arrow navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlShiftArrowUp/Down/Right/Left` with xterm modifier sequences `\u001B[1;6A/B/C/D`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Shift arrows.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
