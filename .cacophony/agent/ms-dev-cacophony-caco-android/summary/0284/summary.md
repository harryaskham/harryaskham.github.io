# bd-460929 WearOS terminal Meta-P/Meta-N helpers

## Goal
Add staged WearOS terminal quick-key helpers for Meta-P and Meta-N while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaP` and `MetaN` with ESC+p / ESC+n input frames.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Meta-P and Meta-N.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
