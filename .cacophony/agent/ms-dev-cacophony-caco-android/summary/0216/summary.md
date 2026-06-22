# bd-b148ee WearOS terminal Meta-Backspace helper

## Goal
Add a staged WearOS terminal quick-key helper for readline backward-kill-word navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.MetaBackspace("Meta-Backspace", "\u001B\u007F")`.
- Pinned the ESC+DEL input frame and label in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include `Meta-Backspace`.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
