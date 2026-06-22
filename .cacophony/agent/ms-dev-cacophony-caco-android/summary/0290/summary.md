# bd-daf95d WearOS terminal Meta-7/Meta-8 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Meta-7 and Meta-8 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.Meta7` and `Meta8` with ESC+7 / ESC+8 input frames.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Meta-7 and Meta-8.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
