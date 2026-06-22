# bd-3a52ef WearOS terminal Ctrl-@ NUL helper

## Goal
Extend the staged WearOS terminal quick-key helper coverage with Ctrl-@ / NUL while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlAt("Ctrl-@", "\u0000")`.
- Pinned the NUL input frame and label in `WatchPtyFramesSourceTest`.
- Regenerated the terminal shell quick-key label expectation so preview labels include `Ctrl-@`.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
