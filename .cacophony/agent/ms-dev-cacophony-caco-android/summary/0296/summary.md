# bd-7dc874 WearOS terminal Meta-G/Meta-H helpers

## Goal
Add staged WearOS terminal quick-key helpers for Meta-G and Meta-H while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaG` and `MetaH` with ESC+g / ESC+h input frames.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Meta-G and Meta-H.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
