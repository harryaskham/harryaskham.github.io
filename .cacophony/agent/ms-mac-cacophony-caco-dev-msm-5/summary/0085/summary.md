# Session summary — Android Agents ms-dev capture

## Goal

Continue Android companion surface testing on the seeded ms-dev emulator by capturing the Agents tab at low resolution and checking the connected state.

## Bead(s)

- `bd-dfdad6` — Android companion: capture Agents screen on ms-dev

## Before state

- The seeded ms-dev emulator flow could build, install, seed host/token config, and capture low-resolution screenshots.
- Agents had not yet been captured in this burn-down sequence.

## After state

- Captured a fresh seeded Overview baseline.
- Navigated to the Agents tab and captured the connected Agents screen.
- The screen shows the top-bar `Agents` label, connected chip, search field, status filters, hero summary (`33 total · 33 running`), project section, and floating create button.
- Harry requested a real-phone install; the safe `adb install -r` attempt reached `sgu24:5555` but stopped on signer mismatch without uninstalling or clearing app data.

## Diff summary

- Commits: summary-only Agents capture
- Files touched: `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-5/summary/0085/**`
- Tests:
  - Remote ms-dev build/install/seed/capture — passed
  - Safe real-phone `adb install -r` — stopped on `INSTALL_FAILED_UPDATE_INCOMPATIBLE`
- Behavioural delta: no app code changed; Agents coverage is added to the recorded Android screenshot set.

## Embedded artefacts

- `screenshots/android-msdev-agents-baseline.png` — seeded Overview baseline.
- `screenshots/android-msdev-agents.png` — Agents tab connected state.

## Operator-takeaway

The Android Agents surface renders correctly in the seeded ms-dev emulator, and the real phone was protected: the install was attempted with `adb install -r` only and stopped at signer mismatch without data loss.
