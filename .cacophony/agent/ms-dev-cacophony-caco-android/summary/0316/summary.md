# bd-e26f2f WearOS terminal quick-key overflow summary

## Goal
Make the WearOS terminal shell's compact quick-key preview explain hidden overflow in plain text while preserving the no-live-input boundary.

## Changes
- Added `watchTerminalShellQuickKeyOverflowSummary(...)` with empty/no-overflow/singular/plural overflow copy.
- Rendered the overflow summary near the staged quick-key preview, count, limit hint, and summary.
- Added source tests for empty, no-overflow, singular overflow, plural overflow, and low visible-limit cases.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
