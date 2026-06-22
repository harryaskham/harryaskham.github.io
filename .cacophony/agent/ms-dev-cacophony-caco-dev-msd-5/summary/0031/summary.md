# Session summary — bd-b0c956 log-monitor version drift evidence

## Goal

Implement `bd-b0c956`: make log-monitor recurrence diagnostics include live caco commit/version evidence so deployment drift is distinguishable from real classifier regressions.

## Changes

- Updated `.cacophony/profiles/log-monitor.md` guidance for classifier-style recurrence diagnostics.
- Log-monitor now must include live runtime version evidence (`caco version --json` or live version/commit) plus relevant source/checkout `HEAD` when filing/updating recurrence evidence or handoff recurring-signature entries.
- Guidance directs log-monitor to classify mismatched live/source commits as deployment/runtime convergence when source already contains the classifier/policy and only old runtime logs are recurring.
- Added caco-profile regression coverage to keep this guidance present in the shipped log-monitor prompt.

## Validation

- `cargo test -p caco-profile --lib log_monitor_persistent_stack_excludes_worker_lifecycle_mixins -- --test-threads=1`
- `git diff --check`

## Diff summary

- Code/content commit: `b177c3fae7`
- Summary artefact commit: omitted intentionally; reintegration receipt is the source for the final landed squash SHA.
