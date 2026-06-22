# Session summary — bd-b9726c Wear OS companion bundled into Play release pipeline

## Goal

Wire the Wear OS companion (`com.cacophony.companion.wear`) into the same
release pipeline as the phone companion (`com.cacophony.companion`) so a
single operator-driven workflow run produces and uploads both signed AABs to
the Google Play internal-testing track, sharing one upload key and one
monotonic versionCode lane.

## Bead(s)

- `bd-b9726c` — Bundle Wear OS companion into the existing Play listing
  alongside `com.cacophony.companion` (Option B).
- Filed during this session: `bd-f55e2d` — Notify operator + offer Play
  internal-testing choice on new companion APK/AAB release (follow-up).

## Before state

- `companion/android/wearable/build.gradle.kts` hardcoded
  `versionCode = 1`, `versionName = "1.0.0"`, and its `release` signing
  config pointed at the debug keystore. Play would reject the wear AAB.
- `:wearable:bundleRelease` was never invoked in
  `.github/workflows/android-companion.yml`; only `:wearable:assembleRelease`
  (APK) ran. There was no Play upload leg for the wear artifact.
- Manifest declared `com.google.android.wearable.standalone = false`
  (bd-77d1f4) even though the watch app now works standalone in operator
  testing.
- `companion/android/QA.md` had no Play Store release section.

## After state

- Wear module shares the phone module's upload-key resolution
  (`CACO_UPLOAD_KEYSTORE` env / `keystore.properties` / debug fallback) and
  switches `release` to the `upload` signing config when material is
  configured. `CACO_ANDROID_PLAY_SIGNING_REQUIRED=1` now also fails fast for
  the wear build when material is absent.
- Wear `versionCode` derives from `git rev-list --count HEAD` and
  `versionName` from the workspace `Cargo.toml` mainline version plus short
  SHA, matching the phone lane exactly. Distinct applicationIds mean shared
  versionCodes do not collide in Play.
- `:wearable:bundleRelease` is built and its AAB signing cert is verified
  against `CACO_EXPECTED_SIGNING_CERT_SHA256` alongside the phone AAB.
- The Play upload step now runs `scripts/play-internal-upload.py` twice in
  one workflow_dispatch run: once for `com.cacophony.companion` and once for
  `com.cacophony.companion.wear`, using the same upload key, the same
  `play_upload_mode` (validate/draft/rollout), and the same
  `play_track` (default `internal`).
- Wear AAB is staged into `dist/cacophony-wearable.aab`, attached to the
  GitHub release on tag pushes, and uploaded as a 1-day preview artifact on
  non-tag runs.
- `com.google.android.wearable.standalone` flipped to `true` with an
  in-manifest comment that records the operator-verified standalone
  behaviour and supersedes the bd-77d1f4 rationale.
- `companion/android/QA.md` now has a "Play Store internal-testing release
  (phone + wear, bd-b9726c)" section documenting the secrets, dispatch
  flow, listing/applicationId caveat, and follow-up to bd-f55e2d.

## Diff summary

- Code commit: pending final squash SHA from reintegration receipt.
- Files touched:
  - `.github/workflows/android-companion.yml` — build `:wearable:bundleRelease`,
    verify wear AAB cert, upload phone + wear AABs to Play in one dispatch,
    stage and publish wear AAB artifact.
  - `companion/android/wearable/build.gradle.kts` — upload-key resolution,
    git-derived versionCode/versionName, `bundle{}` config splits, Play
    signing requirement gate.
  - `companion/android/wearable/src/main/AndroidManifest.xml` — flip
    `standalone` to `true` with updated rationale.
  - `companion/android/QA.md` — new Play release section.
- Tests: no new automated tests; this is build-pipeline plumbing that is
  exercised end-to-end by the `android-companion.yml` workflow_dispatch run
  on the self-hosted Linux runner. JVM unit tests for the wear module
  (`:wearable:testDebugUnitTest`) continue to run unchanged.
- Behavioural delta: a single operator dispatch with
  `play_upload_mode=draft|rollout` and a configured upload key now ships
  both the phone and wear companion AABs to Play internal testing in
  lock-step instead of just the phone AAB.

## Embedded artefacts

- None this session; build pipeline changes are best exercised by the
  workflow dispatch and recorded by `bd-f55e2d` follow-up.

## Operator-takeaway

Play treats `com.cacophony.companion` and `com.cacophony.companion.wear` as
two separate "apps" because they have distinct applicationIds. The
operator-facing contract delivered here is "one workflow_dispatch run
promotes both" — same upload key, same versionCode lane, same track — not
"one Play Console listing with one applicationId" (which would require
merging the wear module into the phone module's applicationId, a much
larger refactor that bd-b9726c did not request). Pair with bd-f55e2d to
flip Play promotion from a manual workflow dispatch into a
choices/notification-driven flow before enabling unattended auto-push.
