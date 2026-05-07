# Session summary — release validation precedence

## Goal

Clarify release-agent validation guidance so routine `update-helper` cadence no longer appears to conflict with shared-host queued validation rules or the legacy one-shot release profile.

## Bead(s)

- `bd-442301` — Profile audit: validation instructions disagree between direct cargo checks and queued heavyweight checks

## Before state

- Failing tests: none known.
- Relevant metrics: `caco-release` still described itself as periodic and compilation-verifying, while `update-helper` had newer no-local/no-queued Cargo cadence policy.
- Context: Profile audit found release agents could read contradictory instructions about direct Cargo, queued Cargo, and release cadence validation.

## After state

- Failing tests: none.
- Relevant metrics: validation passed with `caco build run --wait --command "just docs-build"`, `caco build run --wait --command "just docs-check"`, and local `caco config validate --config .cacophony/config.yaml` (existing warnings only).
- Context: `caco-release` is now explicitly legacy one-shot, routine cadence is assigned to `update-helper`, and queued `cargo check` is only for explicit one-shot pre-release validation on shared hosts.

## Diff summary

- Commits: `58f0a6433`
- Files touched: `.cacophony/profiles/caco-release.md`, `README.md`, `docs/profiles.html`
- Tests: +0 / -0 / flipped 0; docs/profile generated output refreshed.
- Behavioural delta: Release instructions now state the precedence: update-helper cadence does not run local or queued Cargo validation; legacy one-shot release validation uses queued `cargo check --cwd "$PWD"` only when explicitly requested.

## Operator-takeaway

The release-validation policy is now explicit: routine tags keep moving through `update-helper` and GitHub Release binaries, while any exceptional one-shot pre-release compile proof uses the shared-host queue from the agent checkout so uncommitted version edits are visible.
