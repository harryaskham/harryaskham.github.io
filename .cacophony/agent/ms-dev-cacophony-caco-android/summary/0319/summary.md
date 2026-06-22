# bd-015798 WearOS terminal output preview summary

## Goal
Make the WearOS terminal shell's output preview state easier to read on a round display while preserving the no-live-input boundary.

## Changes
- Added `watchTerminalShellOutputPreviewSummary(...)` combining empty/output-size and preview-line-limit state.
- Rendered the summary near the existing terminal output preview, output summary, and preview limit hint.
- Added source tests for empty, all-visible, singular, truncated, and low visible-limit cases.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
