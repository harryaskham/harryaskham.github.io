# bd-845957 WearOS terminal quick-key preview limit hint

## Goal
Make the WearOS terminal shell explain the truncation in the compact quick-key preview while preserving the no-live-input boundary.

## Changes
- Added `watchTerminalShellQuickKeyPreviewLimitHint(...)` with empty/all-visible/truncated copy.
- Rendered the hint near the existing staged quick-key preview and count.
- Added source tests for empty, all-visible, truncated, and low visible-limit cases.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
