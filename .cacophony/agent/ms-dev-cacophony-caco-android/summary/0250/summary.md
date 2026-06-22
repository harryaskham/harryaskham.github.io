# bd-dd6012 WearOS terminal Ctrl-Alt arrow helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Alt arrow navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlAltArrowUp/Down/Right/Left` with xterm modifier sequences `\u001B[1;7A/B/C/D`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Alt arrow labels.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
