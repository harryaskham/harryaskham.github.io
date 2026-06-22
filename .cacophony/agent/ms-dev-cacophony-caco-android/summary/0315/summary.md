# bd-5d0b18 WearOS terminal quick-key summary copy

## Goal
Make the WearOS terminal shell's compact quick-key preview easier to understand while preserving the no-live-input boundary.

## Changes
- Added `watchTerminalShellQuickKeySummary(...)` with empty/all-visible/truncated copy.
- Rendered the summary near the existing staged quick-key preview, count, and limit hint.
- Added source tests for empty, singular, all-visible, truncated, and low visible-limit cases.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
