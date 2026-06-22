# bd-d3c2ad WearOS terminal Meta-V/Meta-Z helpers

## Goal
Add staged WearOS terminal quick-key helpers for Meta-V and Meta-Z while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaV` and `MetaZ` with ESC+v / ESC+z input frames.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`, keeping Meta-Z separate from the existing Ctrl-Z signal quick key.
- Regenerated terminal shell quick-key label expectations so previews include Meta-V and Meta-Z.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
