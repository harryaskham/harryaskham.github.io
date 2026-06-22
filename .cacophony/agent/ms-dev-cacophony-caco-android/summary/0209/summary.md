# bd-a2ca84 Android command-server preferences aliases

## Goal
Add focused Android phone command-server compatibility aliases for the existing Settings screen.

## Changes
- Added `preferences` and `prefs` to Android command-server target discovery.
- Routed those aliases to the existing Settings focus path in `MainActivity`.
- Extended `AndroidRemoteCommandServerSourceTest` to pin discovery and routing/source wiring.

## Validation
- `cd companion/android && nix develop --command gradle :app:testDebugUnitTest --tests 'com.cacophony.companion.AndroidRemoteCommandServerSourceTest' :app:assembleRelease --no-daemon`
