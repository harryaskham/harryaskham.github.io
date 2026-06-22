# bd-9912d6 WearOS terminal endpoint card accessibility copy

## Goal
Make the WearOS terminal preview shell endpoint/connection card clearer for TalkBack users without changing the preview-only/no-live-PTY behavior.

## Changes
- Added `watchTerminalShellEndpointContentDescription(...)` to summarize connection mode, transport, readiness, endpoint security/host, and privacy boundary as one secret-free string.
- Applied the helper to the endpoint card via Compose semantics.
- Added focused source tests for no-config and remote-mTLS copy plus screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`
