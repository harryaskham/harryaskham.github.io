# bd-e0b128 WearOS terminal PageUp/PageDown helpers

## Goal
Add staged WearOS terminal quick-key helpers for standard page navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.PageUp("PageUp", "\u001B[5~")` and `PageDown("PageDown", "\u001B[6~")`.
- Pinned the ESC[5~ / ESC[6~ input frames and labels in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include `PageUp` and `PageDown`.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
