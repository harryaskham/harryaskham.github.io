# bd-20498b WearOS terminal Meta-M/Meta-Q helpers

## Goal
Add staged WearOS terminal quick-key helpers for Meta-M and Meta-Q while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaM` and `MetaQ` with ESC+m / ESC+q input frames.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Meta-M and Meta-Q.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
