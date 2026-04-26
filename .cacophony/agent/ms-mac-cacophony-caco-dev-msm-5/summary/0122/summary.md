# Session summary — Android Profiles capture blocked by More deep-scroll ANR

## Goal

Continue the Android companion ms-dev screenshot sweep by navigating to More -> Profiles and capturing low-resolution simulator evidence.

## Bead(s)

- `bd-79bb92` — Android companion: capture Profiles screen on ms-dev
- `bd-c308c3` — Android companion: Profiles capture navigation can ANR while deep-scrolling More

## Before state

- Failing tests: none known for this worker.
- Relevant metrics: Terminal had just been captured; Profiles remained uncovered and lives lower in More under Configuration.
- Context: Scratchpad was promoted to avoid deep-scroll ANRs, but Profiles still requires reaching the lower More section.

## After state

- Failing tests: no code tests run in this evidence slice.
- Relevant metrics: seeded helper build/install/capture passed. Repeated More swipes to reach Profiles resulted in a `Cacophony isn't responding` dialog before Profiles could be tapped. Filed `bd-c308c3` for the Profiles/deep-More navigation ANR.
- Context: Profiles capture is blocked by the same More depth/scroll fragility pattern that previously blocked Scratchpad.

## Diff summary

- Commits: pending at summary authoring time.
- Files touched: `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-5/summary/0122/summary.md`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-5/summary/0122/screenshots/*.png`
- Tests: remote seeded helper run; adb swipes; UIAutomator text inspection; low-resolution screenshots.
- Behavioural delta: no production code changed; this records the Profiles capture blocker.

## Embedded artefacts

- `screenshots/android-msdev-profiles-overview.png` — seeded helper Overview launch.
- `screenshots/android-msdev-profiles-more.png` — first deep-scroll attempt.
- `screenshots/android-msdev-profiles-more2.png` — second attempt before ANR dialog was observed.

## Operator-takeaway

Profiles is not captured yet. It is blocked by More deep-scroll ANR, so the next fix should promote Profiles to an easier-to-reach section or otherwise reduce More scrolling pressure.
