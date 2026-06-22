# bd-0a29bc Android Settings theme selected-mode summary

## Goal
Add a focused Android Settings appearance polish slice under the alternate theme backlog so the selected theme mode is summarized in operator-facing copy.

## Changes
- Added `settingsThemeModeSelectedSummary(...)` for concise selected-mode copy.
- Rendered the selected-mode summary in the Settings Appearance section.
- Extended `AndroidNordThemeModeSourceTest` to pin helper output and Settings usage.

## Validation
- `cd companion/android && nix develop --command gradle :app:testDebugUnitTest --tests 'com.cacophony.companion.AndroidNordThemeModeSourceTest' :app:assembleRelease --no-daemon`
