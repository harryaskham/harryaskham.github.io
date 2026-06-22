# bd-74a1f0 WearOS terminal raw error accessibility copy

## Goal
Make raw WearOS terminal preview-shell error messages clearer for TalkBack users while preserving the preview-only/no-live-input boundary.

## Changes
- Added `watchTerminalShellRawErrorContentDescription(...)` for explicit raw error detail copy with blank fallback.
- Applied the helper to the conditional raw error message text via Compose semantics.
- Added focused source tests for helper output and screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
