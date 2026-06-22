# bd-261f64 WearOS terminal Meta-A/Meta-E helpers

## Goal
Add staged WearOS terminal quick-key helpers for Meta-A and Meta-E while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaA` and `MetaE` with ESC+a / ESC+e input frames.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Meta-A and Meta-E.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
