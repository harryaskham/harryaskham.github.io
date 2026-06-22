# bd-b51ddb Android command-server theme aliases

## Goal
Add focused Android phone command-server compatibility aliases for the Settings appearance/theme surface.

## Changes
- Added `theme`, `themes`, and `appearance` to Android command-server target discovery.
- Routed those aliases to the existing Settings focus path in `MainActivity`.
- Extended `AndroidRemoteCommandServerSourceTest` to pin discovery and routing/source wiring.

## Validation
- `cd companion/android && nix develop --command gradle :app:testDebugUnitTest --tests 'com.cacophony.companion.AndroidRemoteCommandServerSourceTest' :app:assembleRelease --no-daemon`
