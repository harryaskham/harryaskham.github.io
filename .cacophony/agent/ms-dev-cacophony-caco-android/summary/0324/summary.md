# bd-427184 WearOS terminal status card accessibility copy

## Goal
Make the WearOS terminal preview shell status card clearer for TalkBack users while preserving the preview-only/no-live-PTY boundary.

## Changes
- Added `watchTerminalShellStatusContentDescription(...)` to combine status, selected agent, terminal state, and preview-mode badge copy.
- Applied the helper to the terminal status card via Compose semantics.
- Added focused source tests for helper output and screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
