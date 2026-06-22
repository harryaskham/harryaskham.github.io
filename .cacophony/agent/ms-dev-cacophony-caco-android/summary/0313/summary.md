# bd-17685f WearOS terminal empty quick-key preview copy

## Goal
Make WearOS terminal shell quick-key preview copy readable even for empty/placeholder helper lists.

## Changes
- Updated `watchTerminalShellQuickKeyPreview(emptyList())` to return `none staged` instead of an empty string.
- Added source tests for empty, single-item, overflow, and low visible-limit preview copy.
- Preserved the no-WebSocket/no-live-input safety boundary.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
