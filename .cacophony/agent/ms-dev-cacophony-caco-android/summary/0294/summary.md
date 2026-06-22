# bd-58a279 WearOS terminal Meta-I/Meta-O helpers

## Goal
Add staged WearOS terminal quick-key helpers for Meta-I and Meta-O while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaI` and `MetaO` with ESC+i / ESC+o input frames.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Meta-I and Meta-O.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
