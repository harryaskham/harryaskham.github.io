# bd-ee0c71 WearOS terminal Ctrl-Space NUL alias

## Goal
Add a conventional Ctrl-Space quick-key alias for the WearOS terminal NUL helper while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.CtrlSpace("Ctrl-Space", "\u0000")` beside `Ctrl-@`.
- Pinned the NUL input frame and label in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include `Ctrl-Space`.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
