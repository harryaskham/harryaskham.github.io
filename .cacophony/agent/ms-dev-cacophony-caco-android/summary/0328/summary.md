# bd-e51f3e WearOS terminal header card accessibility copy

## Goal
Make the WearOS terminal preview shell header clearer for TalkBack users while preserving the preview-only/no-live-input boundary.

## Changes
- Added `watchTerminalShellHeaderContentDescription(...)` to summarize title, preview-mode badge, selected agent, and direct-daemon caption as one compact string.
- Applied the helper to the header card via Compose semantics.
- Added focused source tests for normal and blank-agent/header-caption copy plus screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
