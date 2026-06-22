# bd-375386 WearOS terminal Insert/Delete helpers

## Goal
Add staged WearOS terminal quick-key helpers for standard Insert/Delete edit keys while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.Insert("Insert", "\u001B[2~")` and `Delete("Delete", "\u001B[3~")`.
- Pinned the ESC[2~ / ESC[3~ input frames and labels in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include `Insert` and `Delete`.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
