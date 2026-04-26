# Session summary — Android More ANR fixed by disabling tap haptics

## Goal

Investigate and burn down the More-open ANR that blocked Android surface screenshots after the More row hit-target patch, using ms-dev seeded emulator captures for evidence.

## Bead(s)

- `bd-5cbe5a` — Android companion: opening More can ANR after patched debug install on ms-dev
- Related: `bd-3177fb` — Android More row hit targets overlap/confuse QA

## Before state

- Failing tests: none known for this worker; another ms-dev worker owns the bottom-tab Timeline test expectation update.
- Relevant metrics: tapping More after the previous patched debug install produced `Cacophony isn't responding`, UIAutomator null-root output, and logcat showed skipped frames plus input channel disposal. More was not reliably accessible for Speech/Notifications/Web App QA.
- Context: the ANR appeared immediately on navigation taps and was suspected to involve per-tap haptic feedback or More composition cost.

## After state

- Failing tests: local Android `gradle :app:compileDebugKotlin --no-daemon` passed; remote ms-dev `gradle :app:assembleDebug` passed through the QA helper.
- Relevant metrics: after changing `rememberTapHaptic()` to a no-op, the seeded app opened More successfully and then navigated to Notifications successfully. UIAutomator reported the More menu rows and a hydrated Notifications surface with `2 notifications`.
- Context: disabling app-level tap haptics removes the ANR trigger on ms-dev and reopens the Android surface sweep.

## Diff summary

- Commits: pending at summary authoring time.
- Files touched: `companion/android/app/src/main/java/com/cacophony/companion/ui/components/Components.kt`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-5/summary/0095/summary.md`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-5/summary/0095/screenshots/*.png`
- Tests: local `gradle :app:compileDebugKotlin --no-daemon`; remote ms-dev `gradle :app:assembleDebug`; seeded APK install/launch; logcat inspection; UIAutomator dumps; low-resolution screenshots.
- Behavioural delta: shared Android tap-haptic helper is now a no-op, trading tactile feedback for reliable emulator/device navigation.

## Embedded artefacts

- `screenshots/android-msdev-more-anr-investigate.png` — pre-fix ANR dialog and null-root behavior.
- `screenshots/android-msdev-more-no-haptic-overview.png` — clean seeded Overview after the no-haptic build.
- `screenshots/android-msdev-more-no-haptic-open.png` — More menu opening successfully after the fix.
- `screenshots/android-msdev-more-no-haptic-notifications.png` — successful More-row navigation to Notifications after the fix.

## Operator-takeaway

The More ANR was caused or amplified by app-level tap haptics on the emulator path. Disabling shared tap haptics restores reliable More navigation and lets the Android screenshot sweep continue.
