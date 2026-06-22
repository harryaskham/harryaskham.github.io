# bd-da1cbb WearOS terminal staged quick-key count

## Goal
Make the WearOS terminal shell's large staged quick-key set easier to understand without changing the no-live-input boundary.

## Changes
- Added `watchTerminalShellQuickKeyCountSummary(...)` with empty/singular/plural copy.
- Rendered the quick-key count near the existing staged quick-key preview.
- Added source tests for the helper and screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
