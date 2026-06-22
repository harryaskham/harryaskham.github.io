# Session summary — bd-845e9b WearOS DataLayer package identity

## Goal

Fix operator-reported phone↔WearOS pairing failure: the phone app could not send the node token to the watch because the phone and wearable artifacts were installed as separate app packages.

## Bead(s)

- `bd-845e9b` — WearOS DataLayer pairing: align wearable applicationId with phone app

## Before state

- Phone app `applicationId` was `com.cacophony.companion`.
- Wear app `applicationId` was `com.cacophony.companion.wear`.
- Phone→watch daemon-profile sync uses Wear OS DataLayer (`WearRelay.publishDaemonProfile` on `/daemon/profile`; `WatchPhoneDaemonProfile` listens for the same path).
- Distinct packages make phone and watch independent apps, so DataLayer/MessageClient pairing cannot deliver the node token reliably.
- `release-to-play.sh` already used a `wear:*` track but uploaded the wear AAB to `com.cacophony.companion.wear`, contradicting the same-Play-app form-factor track model.

## After state

- Wearable Gradle `applicationId` now matches the phone: `com.cacophony.companion`.
- Wearable Kotlin namespace remains `com.cacophony.companion.wear`, so source packages and component class names stay stable.
- Wear manifest comment now documents shared applicationId + `standalone=false` as the DataLayer pairing contract.
- `release-to-play.sh` uploads the wear AAB to the same package (`$PHONE_PACKAGE`) while keeping the `wear:$TRACK` form-factor track.
- `qa-wear-screenshot.sh` launches the wearable activity as `com.cacophony.companion/com.cacophony.companion.wear.MainActivity`.
- QA / Play Store docs updated to describe one Play package with phone and WearOS form-factor tracks, and to mark the old `com.cacophony.companion.wear` watch package/listing as obsolete.
- Added `WearSharedApplicationIdSourceTest` and updated existing source pins.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `companion/android/wearable/build.gradle.kts`
  - `companion/android/wearable/src/main/AndroidManifest.xml`
  - `companion/android/scripts/release-to-play.sh`
  - `companion/android/scripts/qa-wear-screenshot.sh`
  - `companion/android/QA.md`
  - `companion/android/PLAY_STORE.md`
  - `companion/android/wearable/src/test/java/com/cacophony/companion/WearStandaloneManifestSourceTest.kt`
  - `companion/android/wearable/src/test/java/com/cacophony/companion/wear/WearSharedApplicationIdSourceTest.kt`
  - `companion/android/app/src/test/java/com/cacophony/companion/QaWearScreenshotHelperSourceTest.kt`
- Validation:
  - `bash -n scripts/release-to-play.sh scripts/qa-wear-screenshot.sh`
  - `gradle :wearable:testDebugUnitTest --tests com.cacophony.companion.wear.WearSharedApplicationIdSourceTest --tests com.cacophony.companion.WearStandaloneManifestSourceTest :app:testDebugUnitTest --tests com.cacophony.companion.QaWearScreenshotHelperSourceTest --tests com.cacophony.companion.ReleaseToPlayWearTrackSourceTest :wearable:assembleRelease` — BUILD SUCCESSFUL.
  - `git diff --check` — passed.
- Operational note: testers should uninstall any old `com.cacophony.companion.wear` watch package before installing/updating the shared-package wearable build.

## Operator-takeaway

The WearOS artifact now installs under the same package identity as the phone app while keeping wearable code namespaces stable. This should allow Wear OS DataLayer/MessageClient pairing so the phone can push daemon host/port/token to the watch.
