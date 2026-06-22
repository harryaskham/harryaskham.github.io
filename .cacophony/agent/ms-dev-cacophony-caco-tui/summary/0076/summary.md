# Session summary — Android TTS mute tile subtitle helper

## Goal

Hoist Android TTS mute Quick Settings tile subtitle formatting into a pure helper so unknown/muted/audible copy is source-testable without a live tile runtime.

## Bead(s)

- `bd-364286` — Android TTS mute tile subtitle uses pure helper

## Before state

- Failing tests: none before this slice.
- Relevant metrics: `TtsMuteTileService.renderFromCache` computed subtitle text inline, unlike other tile helpers covered by pure tests.
- Context: focused Android tile source-testability slice; no mute API/policy changes.

## After state

- Failing tests: none in final focused validation.
- Relevant metrics: added `ttsMuteTileSubtitle(known, muted, unknown, mutedLabel, unmutedLabel)` and wired `TtsMuteTileService` to it; tests cover unknown, muted, and audible states.
- Context: tile toggle behavior and string resources unchanged.

## Diff summary

- Code/content commits: `6a86bb0890`; final landed squash SHA comes from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `companion/android/app/src/main/java/com/cacophony/companion/tiles/TileSupport.kt`, `companion/android/app/src/main/java/com/cacophony/companion/tiles/TtsMuteTileService.kt`, `companion/android/app/src/test/java/com/cacophony/companion/tiles/QuickSettingsTilesTest.kt`.
- Tests: `tj-528682b6` passed (`:app:testDebugUnitTest --tests com.cacophony.companion.tiles.QuickSettingsTilesTest`); `bj-e44f9d32` succeeded (`:app:assembleRelease`).

## Operator-takeaway

Android TTS mute tile subtitle behavior is now pinned by pure tests while runtime behavior is unchanged.
