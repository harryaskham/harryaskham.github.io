# bd-06849c WearOS terminal Meta-S helper

## Goal
Add a staged WearOS terminal quick-key helper for Meta-S while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaS` with ESC+s input frame.
- Pinned label and input frame in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Meta-S.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
