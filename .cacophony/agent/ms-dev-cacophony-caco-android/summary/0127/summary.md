# Session summary — Canonical Android/WearOS Play pusher

## Goal

Stop multiple agents from racing the same Android/WearOS Play rollout by documenting one canonical push owner and the required SOPS identity path.

## Bead(s)

- `bd-4be16b` — Android Play rollout: document single canonical pusher

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: multiple workers reacted to tag-trigger rollout rules, causing repeated SOPS decrypt failures and duplicate-version retries after successful pushes.
- Context: Harry directed that routine Play pushes need one canonical pathway; `~/.ssh/caco` is the SOPS key source.

## After state

- Failing tests: none observed in focused validation.
- Relevant metrics: caco-android profile and Android QA runbook now state `ms-dev-cacophony-caco-android` is the default routine Play pusher unless Harry delegates otherwise. Other workers should check `android-release-cadence` and nudge/message the Android owner instead of running `release-to-play.sh` in parallel. The required secret path is `~/.ssh/caco` -> `ssh-to-age` -> `SOPS_AGE_KEY_FILE`, and successful rollouts must record release name, phone/wear versionCodes and edit IDs, plus receipt paths.
- Context: This is documentation/source-test only; no release script behavior changed in this slice.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `.cacophony/profiles/caco-android.md`, `companion/android/QA.md`, `ReleaseToPlayWearTrackSourceTest.kt`
- Tests: `ReleaseToPlayWearTrackSourceTest` now pins canonical pusher and SOPS identity guidance.
- Behavioural delta: future agents should coordinate Play rollout through the Android owner rather than duplicate attempts.

## Operator-takeaway

The Play rollout protocol now has a single documented owner/path, reducing duplicate uploads and SOPS identity noise.
