# Session summary — bd-8fae01 WearOS DirectDaemon cleartext HTTP policy

## Goal

Fix the second likely WearOS Direct mode networking blocker after adding INTERNET permission: DirectDaemon URLs are currently `http://host:port`, so the wearable APK must opt into cleartext traffic just like the phone app.

## Bead(s)

- `bd-8fae01` — WearOS Direct mode: allow cleartext daemon HTTP
- Related: `bd-ad8b8a` — WearOS Direct mode: add missing INTERNET permission

## Before state

- `WatchConnectionConfig.baseUrl` builds direct daemon URLs as `http://$host:$port` and comments describe HTTP bearer mode.
- The phone app manifest already sets `android:usesCleartextTraffic="true"`.
- The WearOS manifest had INTERNET after `bd-ad8b8a` but did not opt into cleartext traffic, so Android 9+ network security policy could still block direct daemon HTTP.

## After state

- Added `android:usesCleartextTraffic="true"` to the wearable `<application>` tag.
- Updated the manifest comment to document that DirectDaemon uses HTTP bearer URLs and that cleartext opt-in is intentional while preserving `standalone=false` for phone DataLayer pairing.
- Added `WatchDirectModeCleartextManifestSourceTest` to pin INTERNET, cleartext opt-in, DirectDaemon HTTP source, and `standalone=false` metadata.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `companion/android/wearable/src/main/AndroidManifest.xml`
  - `companion/android/wearable/src/test/java/com/cacophony/companion/wear/WatchDirectModeCleartextManifestSourceTest.kt`
- Validation:
  - `gradle :wearable:testDebugUnitTest --tests com.cacophony.companion.wear.WatchDirectModeCleartextManifestSourceTest :wearable:assembleRelease` — BUILD SUCCESSFUL.
  - `git diff --check` — passed.
- Behavioural delta: WearOS DirectDaemon HTTP is now allowed by manifest network policy; DataLayer companion pairing metadata unchanged.

## Operator-takeaway

WearOS Direct mode now has both required manifest pieces for current HTTP bearer mode: INTERNET permission and cleartext opt-in. If it still fails after the build reaches the watch, debug endpoint/token/reachability rather than manifest policy.
