# Session summary — companion builds auto-rollout to Play internal testing

## Goal

Operator directive (Harry, 2026-06-21): "Companion builds should ALWAYS roll
out to internal testing track when ready." Stop the per-build operator-gated
`caco choices` promotion prompt for the cacophony Android companion release and
make successful companion runs auto-dispatch the Play internal-testing rollout.

## Bead(s)

- No implementation bead; operator-directed controller config maintenance.
- Mechanism reference: `bd-106817` (the `releases.android_companion.auto_push`
  config field) and `bd-70d094` (the operator-gated companion promotion choice
  this config now bypasses when set to `rollout`).

## Before state

- `projects.cacophony.releases.android_companion.auto_push` was unset →
  defaulted to `off`, so each newly-detected successful `android-companion.yml`
  run presented an operator-only `caco choices` prompt
  (`companion-promote-cacophony-<run_id>`, rollout/draft/skip).
- Operator had to manually resolve every companion build to publish.

## After state

- `.cacophony/releases.yaml` now sets `android_companion.auto_push: rollout`.
- `caco config validate --project-config-dir .cacophony` => `config valid`.
- Once landed + propagated, the daemon's companion-detection block skips the
  operator choice and auto-dispatches the android-companion workflow with
  `play_upload_mode=rollout` for every new successful run (internal-testing).
- The in-flight pending choice (run 27897380490, commit 952b0873) was resolved
  as rollout manually, consistent with the directive.

## Diff summary

- Files touched: `.cacophony/releases.yaml` (+ `android_companion.auto_push:
  rollout` with an explanatory comment; channels unchanged).
- Code/content commit SHA: pending final squash SHA from the reintegration
  receipt.
- Tests: none (config-only; schema acceptance proven by existing unit test
  `android_companion_auto_push_deserializes_and_defaults_off_bd_106817`).
- Behavioural delta: future cacophony companion builds auto-roll to Play
  internal testing with no operator prompt; revert by setting `auto_push: off`.

## Operator-takeaway

Companion Play-internal-testing promotion is now unattended for cacophony
(`auto_push: rollout`). If you ever want the operator gate back, set
`.cacophony/releases.yaml` `android_companion.auto_push: off`.
