# Session summary — caco-android proactive claiming policy

## Goal

Update the persistent `caco-android` profile to match Harry's new operating instruction: this agent should proactively claim ready Android companion beads on each cycle instead of waiting only for explicit handoffs, while still preserving single-owner safety, avoiding epics, and installing future WebView/webapp-wrapped APKs on Harry's phone when that path is built and validated.

## Bead(s)

- `bd-48968e` — Update caco-android to proactively claim ready Android beads

## Before state

- Failing tests: no profile parse failure observed. `just docs-check` was already failing with `docs/profiles.html` generated-output drift from profiles; that broken-on-main class was announced as owned by another worker.
- Relevant metrics: `caco-android` profile frontmatter parsed as `name=caco-android`, `persistent=true`, `reintegration.mode=pr_review`.
- Context: the profile explicitly described itself as not an auto-claim worker, which caused repeated passive monitoring cycles even when ready Android companion tasks existed.

## After state

- Failing tests: profile frontmatter parse still passes. `just docs-check` still reports the known shipped profiles HTML drift; I did not take over that non-Android broken-on-main ownership.
- Relevant metrics: profile now instructs caco-android to claim one ready unowned Android companion task/bug/feature per cycle, excluding epics and already-owned work, then finish/reintegrate/close before checking the next bead.
- Context: the phone-install hygiene now explicitly covers future WebView/webapp-wrapped Android APKs: install the freshly built wrapper on Harry's phone after emulator validation, try in-place install first, and only fresh-reinstall with Harry's explicit approval.

## Diff summary

- Commits: `9edb7e7d5e99` (`bd-48968e: make caco-android proactive`).
- Files touched: `.cacophony/profiles/caco-android.md`.
- Tests: frontmatter YAML parse check passed; `just docs-check` executed and failed with pre-existing/generated profiles HTML drift not changed in this commit.
- Behavioural delta: future caco-android cycles should proactively claim ready Android companion beads instead of leaving unassigned Android work idle, while retaining one-bead-at-a-time and single-owner constraints.

## Operator-takeaway

caco-android is now instructed to act like the Android owner Harry expects: claim ready Android companion work each cycle, keep reintegrating focused slices, and install any future WebView-wrapped Android APK on Harry's phone once it is built and emulator-validated.
