# bd-c5aff6 WearOS terminal preview mode badge

## Goal
Make the WearOS terminal shell immediately communicate that it is a staged preview shell, not a live PTY connection.

## Changes
- Added `watchTerminalShellPreviewModeBadge()` returning `Preview shell · no live PTY`.
- Rendered the badge near the terminal title.
- Added focused source tests for the helper and screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
