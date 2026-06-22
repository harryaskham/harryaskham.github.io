# bd-229666 WearOS terminal Meta-hash/Meta-dollar helpers

## Goal
Add staged WearOS terminal quick-key helpers for Meta-# and Meta-$ while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaHash` and `MetaDollar` with ESC+# / ESC+$ input frames.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Meta-# and Meta-$.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
