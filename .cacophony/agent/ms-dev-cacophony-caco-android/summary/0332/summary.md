# bd-858064 WearOS terminal-state hint accessibility copy

## Goal
Make WearOS terminal preview shell error/closed-state hints clearer for TalkBack users while preserving the preview-only/no-live-input boundary.

## Changes
- Added `watchTerminalShellTerminalStateContentDescription(...)` to summarize terminal error/closed hints with reconnect guidance.
- Applied the helper to the conditional terminal-state hint text via Compose semantics, with a null-safe visible-hint fallback.
- Added focused source tests for error and closed state copy plus screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
