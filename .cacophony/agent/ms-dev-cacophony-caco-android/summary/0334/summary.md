# bd-f8a3cc WearOS terminal phase heartbeat accessibility copy

## Goal
Make the WearOS terminal preview shell phase/heartbeat lifecycle rows clearer for TalkBack users while preserving the preview-only/no-live-input boundary.

## Changes
- Added `watchTerminalShellPhaseHeartbeatContentDescription(...)` to summarize terminal phase and heartbeat state together.
- Applied the helper to the phase text via Compose semantics.
- Added focused source tests for pending/waiting and active/received lifecycle copy plus screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
