# Session summary — Android Actions capture found wrong-destination tap

## Goal

Capture the Actions screen from the promoted More layout on ms-dev, using low-resolution simulator screenshots and the seeded node-token launcher flow.

## Bead(s)

- `bd-625bed` — Android companion: capture Actions from promoted More on ms-dev
- `bd-61a92b` — Android companion: Actions row tap opens Beads after More promotion

## Before state

- Failing tests: none known for this worker.
- Relevant metrics: Jobs, Actions, and Summaries were promoted into the reachable More cluster in bd-6b4148; Actions needed a direct capture pass from that new layout.
- Context: prior Android QA found several More row hit-target and scroll-depth issues, so this pass checked the promoted Actions row specifically.

## After state

- Failing tests: no code tests run in this evidence slice; seeded helper build/install/capture passed.
- Relevant metrics: More shows `Actions` with subtitle `Project actions` in the promoted cluster. Tapping the apparent Actions row center opened `Beads` instead, with UIAutomator showing `Beads`, `3763 total • 30 blocked`, and bead filters. Filed `bd-61a92b` for the wrong-destination tap.
- Context: Actions screen capture remains blocked until the promoted row can be tapped reliably or a direct UIAutomator/test-tag navigation path is used.

## Diff summary

- Commits: pending at summary authoring time.
- Files touched: `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-5/summary/0131/summary.md`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-5/summary/0131/screenshots/*.png`
- Tests: remote seeded helper run; adb navigation/tap; UIAutomator text inspection; low-resolution screenshots.
- Behavioural delta: no production code changed; this records an Actions navigation blocker from the promoted More layout.

## Embedded artefacts

- `screenshots/android-msdev-actions-promoted-overview.png` — seeded helper Overview launch.
- `screenshots/android-msdev-actions-promoted-more.png` — promoted More layout showing Actions.
- `screenshots/android-msdev-actions-promoted-final.png` — wrong-destination Beads screen after tapping the apparent Actions row.

## Operator-takeaway

Actions is visible after the More promotion but not yet reliably tappable: the first full-row tap landed on Beads. The next fix should use exact UIAutomator bounds/test tags and harden the row hit targets if the mismatch reproduces.
