# bd-19d1a2 WearOS terminal quick-key card accessibility copy

## Goal
Make the WearOS terminal preview shell quick-key helper card clearer for TalkBack users while preserving the preview-only/no-live-input boundary.

## Changes
- Added `watchTerminalShellQuickKeyContentDescription(...)` to summarize quick-key readiness, staged helper count, preview, visible-limit hint, summary, and overflow as one compact string.
- Applied the helper to the quick-key card via Compose semantics.
- Added focused source tests for helper output and screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
