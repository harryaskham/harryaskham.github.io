# Session summary — Android Jobs System-section tap verified

## Goal

Fix the Jobs More-scroll ANR by moving Jobs into the top System section, then capture the Jobs screen on ms-dev.

## Bead(s)

- `bd-e32fb5` — Android companion: Jobs capture ANRs while scrolling promoted More
- `bd-c2d35d` — Android companion: capture Jobs from promoted More on ms-dev

## Before state

- Failing tests: none known for this worker.
- Relevant metrics: Summary 0139 hit an ANR while scrolling More toward Jobs, with window focus showing `Application Not Responding: com.cacophony.companion`.
- Context: Actions and Summaries became reliable after moving into the top System section, so Jobs received the same treatment.

## After state

- Failing tests: remote ms-dev Android `gradle :app:compileDebugKotlin --no-daemon` passed; seeded helper build/install/capture passed.
- Relevant metrics: More opened with Jobs visible in the System section. UIAutomator reported `Jobs` bounds `[221,1912][319,1961]` and `Tests and builds` bounds `[221,1961][464,1998]`. Tapping row center opened Jobs, showing `Jobs`, `Tests`, `Builds`, `No Tests`, `No tests jobs have been queued yet.`, and `Refresh`.
- Context: Jobs no longer requires the ANR-prone promoted-cluster scroll path and is captured on ms-dev.

## Diff summary

- Commits: pending at summary authoring time.
- Files touched: `companion/android/app/src/main/java/com/cacophony/companion/MainActivity.kt`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-5/summary/0140/summary.md`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-5/summary/0140/screenshots/*.png`
- Tests: remote ms-dev Kotlin compile; seeded build/install/capture; UIAutomator bounds/text; full-resolution adb row-center tap; low-resolution screenshots.
- Behavioural delta: Jobs moved into the top System section after Summaries, removing the short-scroll ANR path.

## Embedded artefacts

- `screenshots/android-msdev-jobs-system-overview.png` — seeded helper Overview launch.
- `screenshots/android-msdev-jobs-system-more.png` — More with Jobs visible in System section.
- `screenshots/android-msdev-jobs-system-final.png` — final Jobs screen capture.

## Operator-takeaway

Jobs is now reachable and captured from More on ms-dev. The prior Jobs ANR path is resolved by placing Jobs in the deterministic System section with Actions and Summaries.
