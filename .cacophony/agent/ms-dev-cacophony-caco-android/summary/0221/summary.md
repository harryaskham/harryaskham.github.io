# bd-009f15 WearOS terminal Meta-C helper

## Goal
Add a staged WearOS terminal quick-key helper for readline capitalize-word navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaC("Meta-C", "\u001Bc")`.
- Pinned the ESC+c input frame and label in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include `Meta-C`.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
