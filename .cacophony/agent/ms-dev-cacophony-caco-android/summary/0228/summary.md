# bd-2c2a97 WearOS terminal Home/End helpers

## Goal
Add staged WearOS terminal quick-key helpers for standard Home/End navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.Home("Home", "\u001B[H")` and `End("End", "\u001B[F")`.
- Pinned the ESC[H / ESC[F input frames and labels in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include `Home` and `End`.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
