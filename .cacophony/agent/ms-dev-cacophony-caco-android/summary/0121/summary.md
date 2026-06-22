# Session summary — Android Releases app-store rows placeholder

## Goal

Make the Android Releases screen explicitly acknowledge the pending mobile app-store release rows requested by the broad release-status parent, without adding upload or Play actions.

## Bead(s)

- `bd-c0723b` — Android Releases: app-store rows placeholder
- parent context: `bd-aa0724` — caco release list/status: surface Android/WearOS/iOS/watchOS app-store release rows

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: Android Releases showed daemon release jobs, but no UI indication that Google Play / WearOS store rows are pending daemon release-store support.
- Context: This slice is Android UI-only; provider-neutral store rows and CLI/release model work remain future slices.

## After state

- Failing tests: none observed in focused validation.
- Relevant metrics: Android Releases now renders a read-only `App store releases` card under the hero, explaining that store-track rows are pending and naming expected Android rows: Google Play phone `internal` and WearOS `wear:internal`.
- Context: The card explicitly does not upload to Play or trigger releases.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `companion/android/app/src/main/java/com/cacophony/companion/ui/releases/ReleasesScreen.kt`, `companion/android/app/src/test/java/com/cacophony/companion/ReleasesAppStoreRowsPlaceholderSourceTest.kt`
- Tests: added focused Releases source test for placeholder copy and no-upload behavior.
- Behavioural delta: operators see the missing app-store row surface as an explicit pending capability rather than an absent feature.

## Operator-takeaway

Android release UI now makes the Play/WearOS store-row gap visible while waiting for daemon release-store data, and remains strictly read-only.
