# bd-1aa4ed WearOS terminal F1-F4 helpers

## Goal
Add staged WearOS terminal quick-key helpers for xterm F1-F4 function keys while preserving the no-live-input safety boundary.

## Changes
- Added `WatchPtyQuickKey.F1` through `F4` with SS3 sequences `\u001BOP`, `\u001BOQ`, `\u001BOR`, and `\u001BOS`.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include F1-F4.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
