# bd-ca0a99 WearOS terminal Meta-B helper

## Goal
Add a staged WearOS terminal quick-key helper for readline backward-word navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaB("Meta-B", "\u001Bb")`.
- Pinned the ESC+b input frame and label in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include `Meta-B`.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
