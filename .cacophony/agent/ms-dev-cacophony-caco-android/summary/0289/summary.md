# bd-ddc251 WearOS terminal Meta-5/Meta-6 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Meta-5 and Meta-6 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.Meta5` and `Meta6` with ESC+5 / ESC+6 input frames.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Meta-5 and Meta-6.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
