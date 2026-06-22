# bd-b38aca WearOS terminal Ctrl-Insert/Ctrl-Delete helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Insert/Ctrl-Delete edit keys while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlInsert("Ctrl-Insert", "\u001B[2;5~")` and `CtrlDelete("Ctrl-Delete", "\u001B[3;5~")`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Insert/Delete.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
