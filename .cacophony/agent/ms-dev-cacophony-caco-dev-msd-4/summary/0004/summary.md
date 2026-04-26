# Session summary — Android More nav tap target

## Goal

Fix an Android companion navigation regression where tapping the bottom-nav More item on the ms-dev emulator could fall through to the launcher/system gesture region instead of opening the More screen.

## Bead(s)

- `bd-45fc4e` — Android companion: bottom nav More tap can drop to launcher on ms-dev

## Before state

- Failing tests: none at bead start.
- Relevant metrics: QA evidence showed a tap around the More bottom-tab bounds dropping to launcher app labels after Timeline had moved out of the bottom bar.
- Context: the visible bottom navigation had six items, but the decorative selected-tab rail was still laid out across all `Tab.entries`, including hidden `Timeline`, and the `NavigationBar` did not explicitly apply navigation-bar inset padding.

## After state

- Failing tests: none in validation.
- Relevant metrics: the rail now uses the same visible tab set as the `NavigationBar`, and the nav bar applies `navigationBarsPadding()` so tap targets stay above the system gesture/nav region.
- Context: Timeline remains available under More, while More’s bottom-nav item gets safer geometry on 1080x2400 emulator layouts.

## Diff summary

- Commits: `fb58f6a5d`
- Files touched: `companion/android/app/src/main/java/com/cacophony/companion/MainActivity.kt`
- Tests: `cd companion/android && nix develop -c gradle :app:testDebugUnitTest --tests com.cacophony.companion.FullAppNavigationTest --no-daemon`; `cd companion/android && nix develop -c gradle :app:testDebugUnitTest --no-daemon`; `git diff --check`
- Behavioural delta: bottom nav no longer lays supporting chrome for hidden Timeline, and More’s hit target avoids the system nav area.

## Operator-takeaway

The Android companion More tab should be less likely to trigger the launcher/system gesture area on ms-dev; the full companion unit-test gate is green after the inset and visible-tab-set fix.
