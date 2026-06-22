# bd-86c419 WearOS terminal Ctrl-Alt Page helpers

## Goal
Add staged WearOS terminal quick-key helpers for Ctrl-Alt-PageUp/PageDown navigation while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlAltPageUp("Ctrl-Alt-PageUp", "\u001B[5;7~")` and `CtrlAltPageDown("Ctrl-Alt-PageDown", "\u001B[6;7~")`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Ctrl-Alt PageUp/Down.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
