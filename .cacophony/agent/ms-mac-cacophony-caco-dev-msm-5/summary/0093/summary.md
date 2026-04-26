# Session summary — Android Notifications surface captured on ms-dev

## Goal

Continue the Android companion surface sweep on ms-dev with the seeded node-token launcher, targeting More → Notifications and filing issues found during real emulator navigation.

## Bead(s)

- `bd-10ba1c` — Android companion: capture Notifications screen on ms-dev
- Follow-up filed: `bd-3177fb` — Android More: Speech row and Notifications row tap targets overlap/confuse QA
- Follow-up queued/filed: Android companion bottom-nav More tap can drop to launcher on ms-dev

## Before state

- Failing tests: none known.
- Relevant metrics: `bd-305ca3` was already claimed by caco-android, so this worker avoided duplicating the emulator restart helper fix and proceeded once the seeded helper could launch the app.
- Context: the ms-dev emulator launched a clean Overview with live daemon data via the seeded node-token APK path.

## After state

- Failing tests: local Android `gradle :app:compileDebugKotlin --no-daemon` passed.
- Relevant metrics: Notifications surface captured successfully; UIAutomator reported `No Notifications` and `Daemon notifications will appear here`, with the top bar title `Notifications` and connected footer state.
- Context: navigation uncovered two QA/product ergonomics issues: one More tap coordinate fell through to the Android launcher, and an imprecise Notifications-row tap opened Speech first before a lower exact tap opened Notifications.

## Diff summary

- Commits: pending at summary authoring time.
- Files touched: `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-5/summary/0093/summary.md`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-5/summary/0093/screenshots/*.png`
- Tests: `nix develop .#android --command bash -lc 'cd companion/android && gradle :app:compileDebugKotlin --no-daemon'`; seeded ms-dev APK install/launch; UIAutomator dumps; low-resolution screenshots.
- Behavioural delta: no production code changed; this slice produced durable screenshot coverage for Notifications and filed navigation follow-ups.

## Embedded artefacts

- `screenshots/android-msdev-notifications-overview.png` — clean seeded Overview after helper launch.
- `screenshots/android-msdev-notifications-more-tap.png` — first More tap attempt that did not switch pages, useful for hit-target diagnosis.
- `screenshots/android-msdev-notifications-more-open.png` — More menu with Speech and Notifications rows visible.
- `screenshots/android-msdev-notifications-open.png` — imprecise Notifications tap opened Speech, evidence for `bd-3177fb`.
- `screenshots/android-msdev-notifications-open-exact.png` — successful Notifications empty-state capture.

## Operator-takeaway

Notifications itself is readable and captured, but Android More navigation remains easy to mis-tap on the emulator; future QA should use UIAutomator bounds or fix the row/hit-target ergonomics before relying on coordinate-only automation.
