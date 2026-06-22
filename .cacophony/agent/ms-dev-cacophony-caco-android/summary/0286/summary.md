# bd-ba851e WearOS terminal Meta-W/Meta-X helpers

## Goal
Add staged WearOS terminal quick-key helpers for Meta-W and Meta-X while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaW` and `MetaX` with ESC+w / ESC+x input frames.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Meta-W and Meta-X.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
