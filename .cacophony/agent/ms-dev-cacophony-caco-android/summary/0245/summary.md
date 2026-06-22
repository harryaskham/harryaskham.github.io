# bd-d075e2 WearOS terminal Alt-Insert/Alt-Delete helpers

## Goal
Add staged WearOS terminal quick-key helpers for Alt-Insert/Alt-Delete edit keys while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.AltInsert("Alt-Insert", "\u001B[2;3~")` and `AltDelete("Alt-Delete", "\u001B[3;3~")`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Alt-Insert/Delete.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
