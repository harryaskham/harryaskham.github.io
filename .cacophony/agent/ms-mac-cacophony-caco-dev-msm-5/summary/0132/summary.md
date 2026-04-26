# Session summary — Android Actions exact-bounds repro hit ANR

## Goal

Investigate the promoted Actions wrong-destination tap by reproducing from ms-dev with exact UIAutomator bounds before changing code.

## Bead(s)

- `bd-61a92b` — Android companion: Actions row tap opens Beads after More promotion
- `bd-578da8` — Android companion: Actions exact-bounds repro hits ANR before row dump

## Before state

- Failing tests: none known for this worker.
- Relevant metrics: Summary 0131 showed Actions visible in More, but tapping the apparent row opened Beads instead. The next step was to collect exact Actions bounds.
- Context: More has a history of short-scroll and deep-scroll ANRs on ms-dev, so this investigation used low-resolution screenshots and UIAutomator text rather than reading large images.

## After state

- Failing tests: no code tests run in this evidence slice; seeded helper build/install/capture passed.
- Relevant metrics: During the attempt to navigate More and dump promoted Actions bounds, the app showed a `Cacophony isn't responding` dialog before the Actions row bounds were available. Filed `bd-578da8` for the ANR-before-bounds blocker.
- Context: The Actions issue appears broader than just one bad tap coordinate; the promoted Actions path can still ANR before deterministic row targeting.

## Diff summary

- Commits: pending at summary authoring time.
- Files touched: `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-5/summary/0132/summary.md`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-5/summary/0132/screenshots/*.png`
- Tests: remote seeded helper run; adb navigation/swipe; UIAutomator text inspection; low-resolution screenshots.
- Behavioural delta: no production code changed; this records that bd-61a92b is blocked by an ANR during exact-bounds reproduction.

## Embedded artefacts

- `screenshots/android-msdev-actions-bounds-overview.png` — seeded helper Overview launch.
- `screenshots/android-msdev-actions-bounds-more.png` — More navigation attempt that resulted in the ANR dialog.

## Operator-takeaway

Actions still needs work: before we can distinguish row-coordinate error from hit-target overlap, the promoted Actions path itself can ANR on ms-dev. A more deterministic navigation strategy or more aggressive More simplification is needed next.
