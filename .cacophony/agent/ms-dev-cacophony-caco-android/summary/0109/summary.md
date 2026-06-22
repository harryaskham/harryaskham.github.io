# Session summary — WearOS Suggestions image prompt placeholder

## Goal

Add a WearOS Suggestions placeholder for the future image-input-to-caco-suggest flow, without adding any run action or upload behavior.

## Bead(s)

- `bd-121ada` — WearOS Suggestions: image input placeholder
- parent context: `bd-ae6b1d` / `bd-174386` — caco suggest wearable/widget surfaces and mesh image input

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: WearOS Suggestions was read-only and had screen/tile/complication support, but no image prompt affordance. Android image-input placeholder is separately owned by another worker as `bd-f9db32`.
- Context: This slice intentionally preserves the read-only/no-run invariants.

## After state

- Failing tests: none observed in focused validation.
- Relevant metrics: WatchSuggestionsScreen now includes an Image prompt chip. Tapping it shows a placeholder message that no image is uploaded and no suggestion is run yet; future work will attach an image through the existing caco file API.
- Context: No POST `/api/v1/suggest`, run helper, image pick, byte copy, or upload is introduced.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `companion/android/wearable/src/main/java/com/cacophony/companion/wear/suggest/WatchSuggestionsScreen.kt`, `companion/android/wearable/src/test/java/com/cacophony/companion/wear/WatchSuggestionsImagePromptPlaceholderSourceTest.kt`
- Tests: added focused WearOS source test for placeholder copy and read-only invariants.
- Behavioural delta: WearOS Suggestions now advertises the future image prompt workflow without changing execution behavior.

## Operator-takeaway

WearOS Suggestions now has a visible image-input placeholder, keeping parity with the broader mobile image/suggest roadmap while remaining read-only.
