# bd-7d7fae WearOS terminal Meta-1/Meta-2 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Meta-1 and Meta-2 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.Meta1` and `Meta2` with ESC+1 / ESC+2 input frames.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Meta-1 and Meta-2.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
