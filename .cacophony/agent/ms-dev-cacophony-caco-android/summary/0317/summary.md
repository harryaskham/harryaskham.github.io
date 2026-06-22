# bd-eaacc3 WearOS terminal preview safety summary

## Goal
Make the WearOS terminal shell's no-live-input boundary more explicit in the on-watch copy.

## Changes
- Added `watchTerminalShellSafetySummary()` with concise preview-only/no-socket/no-input text.
- Rendered the safety summary near the existing follow-up-slice warning.
- Added source tests for the helper and screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
