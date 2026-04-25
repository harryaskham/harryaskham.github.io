# Session summary — Android Feed ms-dev capture

## Goal

Continue Android companion surface testing on the seeded ms-dev emulator by capturing the Feed tab at low resolution while avoiding oversized image reads.

## Bead(s)

- `bd-bc62de` — Android companion: capture Feed screen on ms-dev
- `bd-72e053` — Pi: recover gracefully from 413 payload-too-large after image reads

## Before state

- The ms-dev emulator flow could build, install, seed host/token config, and capture screenshots for Overview, More, Inbox, Beads, Timeline, and Agents.
- The operator warned that reading the next image in-session would trigger a 413, so this pass avoided rendering the screenshot in chat and used file metadata instead.

## After state

- Captured the Feed tab screenshot into the recorded summary directory.
- Confirmed the screenshot exists as a low-resolution PNG without reading it into the model context.
- Observed the remote helper recovered from a version-downgrade install by reinstalling successfully on the emulator; real-phone data was not touched.
- Filed `bd-72e053` for robust Pi recovery from oversized image reads.

## Diff summary

- Commits: summary-only Feed capture plus 413 recovery bead filing
- Files touched: `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-5/summary/0086/**`
- Tests:
  - Remote ms-dev build/install/seed/capture — passed
  - Screenshot metadata check — passed without image read
- Behavioural delta: no app code changed; Feed coverage is added to recorded Android screenshots.

## Embedded artefacts

- `screenshots/android-msdev-feed-baseline.png` — seeded baseline before Feed navigation.
- `screenshots/android-msdev-feed.png` — Feed tab capture, intentionally not rendered into this session to avoid a 413.

## Operator-takeaway

Feed screenshot capture succeeded, and the workflow adapted to the oversized-image risk by recording the artefact without reading it into context. The Pi 413 recovery gap is now tracked as `bd-72e053`.
