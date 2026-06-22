# bd-964251 WearOS terminal input area accessibility copy

## Goal
Make the WearOS terminal preview shell input/focus area clearer for TalkBack users while preserving the preview-only/no-live-input boundary.

## Changes
- Added `watchTerminalShellInputContentDescription(...)` to summarize input status, mode, readiness, focus, read-only hint when present, and reconnect state as one compact string.
- Applied the helper to the input status text via Compose semantics.
- Added focused source tests for staged and live-writable input copy plus screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
