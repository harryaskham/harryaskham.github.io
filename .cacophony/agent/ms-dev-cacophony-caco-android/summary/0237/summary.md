# bd-640ffc WearOS terminal Ctrl-Page helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-PageUp/PageDown navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlPageUp("Ctrl-PageUp", "\u001B[5;5~")` and `CtrlPageDown("Ctrl-PageDown", "\u001B[6;5~")`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-PageUp/Down.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
