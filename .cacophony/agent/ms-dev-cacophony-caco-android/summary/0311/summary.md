# bd-ba401e WearOS terminal Meta-Tab helper

## Goal
Add a staged WearOS terminal quick-key helper for Meta-Tab while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaTab` with ESC+Tab input frame.
- Pinned label and input frame in `WatchPtyFramesSourceTest`, distinct from Tab/Ctrl-I and Shift-Tab.
- Regenerated terminal shell quick-key label expectations so previews include Meta-Tab.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
