# bd-cac620 WearOS terminal output preview accessibility copy

## Goal
Make the WearOS terminal preview shell output area clearer for TalkBack users while preserving the preview-only/no-live-input boundary.

## Changes
- Added `watchTerminalShellOutputContentDescription(...)` to summarize output emptiness/line counts and preview limits as one compact string.
- Applied the helper to the terminal output preview text via Compose semantics.
- Added focused source tests for empty and multi-line output copy plus screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
