# bd-a1bbc9 WearOS terminal action chip accessibility copy

## Goal
Make the WearOS terminal preview shell Back and Connection settings chips clearer for TalkBack users.

## Changes
- Added `watchTerminalShellBackChipContentDescription()` and `watchTerminalShellSettingsChipContentDescription()` helpers.
- Applied the helpers to the bottom action chips via Compose semantics.
- Added focused source tests for helper output and screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
