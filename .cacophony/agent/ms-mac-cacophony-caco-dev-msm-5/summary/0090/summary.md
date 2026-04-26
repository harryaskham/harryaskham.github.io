# Session summary — Android Speech attempt uncovered Timeline/System UI ANR

## Goal

Continue Harry's Android companion ms-dev emulator QA sweep after the seeded node-token launcher work, targeting the More → Speech surface and filing/fixing anything that blocked reliable screenshot capture.

## Bead(s)

- `bd-fdd9a2` — Android companion: capture Speech screen on ms-dev
- `bd-75c11c` — Android companion: Timeline renders too many heavy event bodies and ANRs
- Follow-up filed: `bd-a3dcef` — Android QA helper: recover emulator System UI ANR before capture

## Before state

- Failing tests: none at the start of this slice.
- Relevant metrics: the previous Releases slice made More easier to target, but Timeline was still reachable and the ms-dev emulator had shown repeated ANR dialogs when Timeline rendered a live dataset around 800+ events.
- Context: the plan was to capture Speech, but the first navigation attempts showed persistent ANR state from Timeline/System UI rather than a clean More page.

## After state

- Failing tests: local Android Kotlin compile passed after code changes; remote ms-dev debug builds passed for the Timeline/MainActivity patches.
- Relevant metrics: Timeline was changed to request fewer commit-heavy events, show only the first 12 rows initially, clamp title/body text, and remove Timeline from the bottom navigation while keeping it in More. Despite that, the emulator's System UI remained wedged from the prior heavy Timeline run and contaminated later captures.
- Context: Speech capture is not complete yet; the blocking emulator recovery gap is now tracked separately as `bd-a3dcef`.

## Diff summary

- Commits: pending at summary authoring time.
- Files touched: `companion/android/app/src/main/java/com/cacophony/companion/MainActivity.kt`, `companion/android/app/src/main/java/com/cacophony/companion/ui/timeline/TimelineScreen.kt`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-5/summary/0090/summary.md`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-5/summary/0090/screenshots/*.png`
- Tests: local `gradle :app:compileDebugKotlin --no-daemon`; multiple remote ms-dev `gradle :app:assembleDebug` runs; emulator install/seed/launch attempts; UIAutomator text inspection; low-resolution screenshots.
- Behavioural delta: Timeline is no longer a bottom-tab destination, reducing accidental primary-nav access to the heaviest Android surface; its list rendering is also bounded and text-clamped for follow-up validation once the emulator is clean.

## Embedded artefacts

- `screenshots/android-msdev-speech-baseline.png` — seeded baseline capture for the Speech QA slice.
- `screenshots/android-msdev-timeline-capped-build.png` — build/install evidence after the first Timeline cap attempt.
- `screenshots/android-msdev-timeline-capped40.png` and `screenshots/android-msdev-timeline-no-mincommits.png` — captures showing the ANR persisted while iterating Timeline bounds.
- `screenshots/android-msdev-primary-nav-after-timeline-hide.png` — post-build capture after hiding Timeline from bottom nav.
- `screenshots/android-msdev-more-after-timeline-hide.png` and `screenshots/android-msdev-clean-overview-after-restart.png` — evidence that System UI ANR persisted and needs helper-level recovery.

## Operator-takeaway

This pass found that Timeline can wedge the emulator hard enough to poison later Android captures. I reduced Timeline's primary-nav blast radius and filed a helper recovery bead; next Android QA should restart/clean the emulator before continuing Speech/WebApp captures.
