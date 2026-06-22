# bd-4ad19c WearOS command-server theme aliases

## Goal
Add focused WearOS command-server compatibility aliases for the existing Settings destination's appearance/theme entry point.

## Changes
- Added `theme`, `themes`, and `appearance` to WearOS command-server target discovery.
- Routed those aliases to the existing Settings destination in `MainActivity`.
- Extended `WatchRemoteCommandServerSourceTest` to pin discovery and routing/source wiring.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchRemoteCommandServerSourceTest' :wearable:assembleRelease --no-daemon`
