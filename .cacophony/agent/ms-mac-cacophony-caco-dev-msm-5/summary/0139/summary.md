# Session summary — Android Jobs capture hit More scroll ANR

## Goal

Capture the Jobs screen from the promoted More layout on ms-dev using low-resolution simulator screenshots.

## Bead(s)

- `bd-c2d35d` — Android companion: capture Jobs from promoted More on ms-dev
- `bd-e32fb5` — Android companion: Jobs capture ANRs while scrolling promoted More

## Before state

- Failing tests: none known for this worker.
- Relevant metrics: Jobs had been promoted above the deeper Work Items section, but had not yet been captured directly from the promoted More layout.
- Context: Actions and Summaries were moved into the System section for deterministic targeting after scroll/tap ambiguity; Jobs still required a short scroll.

## After state

- Failing tests: no code tests run in this evidence slice; seeded helper build/install/capture passed.
- Relevant metrics: Opening More and swiping toward Jobs produced an ANR state. UIAutomator only returned `More`, and `dumpsys window` showed `mCurrentFocus=Window{... Application Not Responding: com.cacophony.companion}` while `mFocusedApp` remained MainActivity. Filed `bd-e32fb5` for the Jobs/More ANR.
- Context: Jobs capture remains blocked until Jobs is made deterministic/top-level enough to avoid More scrolling pressure.

## Diff summary

- Commits: pending at summary authoring time.
- Files touched: `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-5/summary/0139/summary.md`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-5/summary/0139/screenshots/*.png`
- Tests: remote seeded helper run; adb More navigation/swipe; UIAutomator and window-focus inspection; low-resolution screenshots.
- Behavioural delta: no production code changed; this records a Jobs capture blocker.

## Embedded artefacts

- `screenshots/android-msdev-jobs-promoted-overview.png` — seeded helper Overview launch.
- `screenshots/android-msdev-jobs-promoted-more.png` — More/ANR state while attempting to reach Jobs.

## Operator-takeaway

Jobs is still not safe to capture from a scrolled More path. It likely needs the same deterministic System-section treatment as Actions and Summaries before the Jobs screen can be captured reliably.
