# bd-2c6593 WearOS terminal preview next-step hint

## Goal
Add a concise next-step hint to the WearOS terminal preview shell while preserving the no-live-input/no-WebSocket boundary.

## Changes
- Added `watchTerminalShellNextStepHint(...)` for missing direct-daemon config, missing agent id, and normal preview-ready states.
- Rendered the hint near the existing follow-up-slice and safety copy.
- Added focused source tests for the helper and screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
