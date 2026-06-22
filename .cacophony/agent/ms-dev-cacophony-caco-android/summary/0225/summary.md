# bd-cafcc9 WearOS terminal Meta-? helper

## Goal
Add a staged WearOS terminal quick-key helper for readline possible-completions while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaQuestionMark("Meta-?", "\u001B?")`.
- Pinned the ESC+? input frame and label in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include `Meta-?`.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
