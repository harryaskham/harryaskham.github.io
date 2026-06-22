# bd-caceed WearOS terminal Meta-L helper

## Goal
Add a staged WearOS terminal quick-key helper for readline lowercase-word navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaL("Meta-L", "\u001Bl")`.
- Pinned the ESC+l input frame and label in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include `Meta-L`.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
