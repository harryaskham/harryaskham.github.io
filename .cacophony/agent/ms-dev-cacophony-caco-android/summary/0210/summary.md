# bd-d85eea WearOS command-server preferences aliases

## Goal
Add focused WearOS command-server compatibility aliases for the existing Settings destination.

## Changes
- Added `preferences` and `prefs` to WearOS command-server target discovery.
- Routed those aliases to the existing Settings destination in `MainActivity`.
- Extended `WatchRemoteCommandServerSourceTest` to pin discovery and routing/source wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchRemoteCommandServerSourceTest' :wearable:assembleRelease --no-daemon`
