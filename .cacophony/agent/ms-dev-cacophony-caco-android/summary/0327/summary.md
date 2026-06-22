# bd-1020fe WearOS terminal safety card accessibility copy

## Goal
Make the WearOS terminal preview shell safety/next-step card clearer for TalkBack users while preserving the preview-only/no-live-input boundary.

## Changes
- Added `watchTerminalShellSafetyContentDescription(...)` to summarize preview safety and next action as one compact string.
- Applied the helper to the safety/next-step card via Compose semantics.
- Added focused source tests for no-config and configured-agent copy plus screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
