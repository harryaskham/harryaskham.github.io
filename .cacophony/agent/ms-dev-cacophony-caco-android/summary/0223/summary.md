# bd-2346cc WearOS terminal Meta-R helper

## Goal
Add a staged WearOS terminal quick-key helper for readline revert-line navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaR("Meta-R", "\u001Br")`.
- Pinned the ESC+r input frame and label in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include `Meta-R`.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
