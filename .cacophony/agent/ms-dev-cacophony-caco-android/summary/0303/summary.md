# bd-bc7b06 WearOS terminal Meta-plus/Meta-underscore helpers

## Goal
Add staged WearOS terminal quick-key helpers for Meta-+ and Meta-_ while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaPlus` and `MetaUnderscore` with ESC++ / ESC+_ input frames.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Meta-+ and Meta-_.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
