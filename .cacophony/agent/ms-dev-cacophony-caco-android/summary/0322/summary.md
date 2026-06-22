# bd-c748d0 WearOS terminal connection transport summary

## Goal
Add concise transport summary copy to the WearOS terminal preview shell while preserving the no-WebSocket/no-live-input boundary.

## Changes
- Added `watchTerminalShellTransportSummary(...)` for no-config, local direct-daemon, and remote mTLS transport states.
- Rendered transport summary near existing connection/endpoint copy.
- Added focused source tests for helper output and screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
