# bd-0b712d WearOS terminal Ctrl-Home/Ctrl-End helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Home/Ctrl-End navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlHome("Ctrl-Home", "\u001B[1;5H")` and `CtrlEnd("Ctrl-End", "\u001B[1;5F")`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Home/End.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
