# bd-5a4b4f WearOS terminal Meta-T helper

## Goal
Add a staged WearOS terminal quick-key helper for readline transpose-words navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaT("Meta-T", "\u001Bt")`.
- Pinned the ESC+t input frame and label in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include `Meta-T`.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
