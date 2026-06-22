# bd-615126 WearOS terminal Ctrl-Alt-Home/End helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Alt-Home/Ctrl-Alt-End navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlAltHome("Ctrl-Alt-Home", "\u001B[1;7H")` and `CtrlAltEnd("Ctrl-Alt-End", "\u001B[1;7F")`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Alt Home/End.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
