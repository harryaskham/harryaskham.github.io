# bd-9bc3a7 WearOS terminal Meta-J/Meta-K helpers

## Goal
Add staged WearOS terminal quick-key helpers for Meta-J and Meta-K while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaJ` and `MetaK` with ESC+j / ESC+k input frames.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Meta-J and Meta-K.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
