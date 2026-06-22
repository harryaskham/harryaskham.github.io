# bd-5775e2 WearOS terminal Meta-F helper

## Goal
Add a staged WearOS terminal quick-key helper for readline forward-word navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaF("Meta-F", "\u001Bf")`.
- Pinned the ESC+f input frame and label in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include `Meta-F`.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
