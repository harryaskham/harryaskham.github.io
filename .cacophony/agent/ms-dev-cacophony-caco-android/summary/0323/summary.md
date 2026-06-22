# bd-8aada2 Android Files image draft summary accessibility copy

## Goal
Make the selected local image draft summary in Android Files clearer for TalkBack users.

## Changes
- Added `filesImageShareDraftContentDescription(...)` wrapping the existing local draft summary.
- Applied the content description to the selected image draft summary text.
- Added focused source tests for the helper and screen wiring.

## Validation
- `cd companion/android && nix develop --command gradle :app:testDebugUnitTest --tests 'com.cacophony.companion.FilesImageSharePlaceholderSourceTest' :app:assembleRelease --no-daemon`
