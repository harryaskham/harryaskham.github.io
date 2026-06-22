# bd-626f5f WearOS Releases row accessibility copy

## Goal
Make WearOS Releases rows clearer for TalkBack users without changing release fetch/cancel behavior.

## Changes
- Added `watchReleaseRowContentDescription(...)` to summarize release id, state, channel, strategy, commit, and cancelability.
- Applied the helper to each release row/chip via Compose semantics.
- Added focused source tests for cancelable and non-cancelable release rows plus screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchReleasesSourceTest' :wearable:assembleRelease --no-daemon`
- Reran source test after adding screen-wiring assertions: `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchReleasesSourceTest' --no-daemon`
