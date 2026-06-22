# bd-a50c2a WearOS terminal Meta-3/Meta-4 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Meta-3 and Meta-4 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.Meta3` and `Meta4` with ESC+3 / ESC+4 input frames.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Meta-3 and Meta-4.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
