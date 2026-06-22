# bd-1c8328 WearOS terminal input readiness summary

## Goal
Make the WearOS terminal shell input readiness easier to understand while preserving the no-live-input boundary.

## Changes
- Added `watchTerminalShellInputReadinessSummary(...)` for idle, live writable, read-only, and error states.
- Rendered the input readiness summary near the existing input and focus copy.
- Added focused source tests for the helper and screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
