# Session summary — caco-transcription lifecycle hook cleanup

## Goal

Remove completion-only lifecycle hooks from the endless `caco-transcription` persistent profile so its resolved profile matches its own reintegrate-only lifecycle contract.

## Bead(s)

- `bd-00c37d` — Remove completion hook from endless caco-transcription profile

## Before state

- Failing tests: none specific to this bead at start.
- Relevant metrics: `.cacophony/profiles/caco-transcription.md` declared `lifecycle: endless`, `persistent: true`, `allowed_lifecycle_operations: [reintegrate]`, and prompt text saying not to call `caco agent complete`, but still composed the `completion` hook mixin.
- Context: This made the profile internally contradictory: completion-only hook phases could be materialized for a persistent specialist that should reintegrate and continue.

## After state

- Failing tests: none from focused validation.
- Relevant metrics: queued job `tj-3d4cae67` passed `cargo test -p caco-profile caco_transcription_endless_profile_does_not_compose_completion_hooks_bd_00c37d -- --nocapture`.
- Context: `caco-transcription` now composes only `base-lifecycle` and `worker-tools`; a regression test loads the checked-in profile and asserts it remains endless/reintegrate-only and does not inherit `before_complete`/`on_complete` phases.

## Diff summary

- Code/content commits: `99272bd9e4`
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `.cacophony/profiles/caco-transcription.md`, `crates/caco-profile/tests/profile.rs`
- Tests: +1 / -0 / flipped 0; focused queued caco-profile test passed in `tj-3d4cae67`
- Behavioural delta: the transcription persistent profile no longer gets completion-hook guidance or completion lifecycle hook phases.

## Operator-takeaway

The caco-transcription profile now has a consistent endless lifecycle: it can reintegrate work and continue, without carrying one-shot completion hook wiring that conflicts with its prompt and allowed operations.
