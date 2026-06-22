# bd-3982c9 WearOS terminal Meta-9/Meta-0 helpers

## Goal
Add staged WearOS terminal quick-key helpers for Meta-9 and Meta-0 while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.Meta9` and `Meta0` with ESC+9 / ESC+0 input frames.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Meta-9 and Meta-0.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
