# Session summary — bd-535ad2 handoff successor replay plan

## Goal

Address `bd-535ad2`: replay dirty diff, summary artifacts, and provenance into an isolated successor worktree after worktree creation exists. Spawn and memo generation remain out of scope.

## Changes

- Added `HandoffSuccessorReplayPlan`.
- Added `plan_handoff_successor_replay(...)`, a pure replay/materialization plan for resolved source + successor worktree.
- Plan captures whether to apply dirty patch, patch byte count, bounded non-empty artifact paths, and deterministic provenance/summary paths in the worktree.
- Clean-checkpoint sources do not apply dirty patches even if patch text is supplied.
- Added regression for dirty replay planning, artifact filtering, provenance paths, and clean-source no-dirty behavior.

## Validation

- `cargo test -p caco-daemon --lib plan_handoff_successor_replay_captures_dirty_artifacts_bd_535ad2 -- --test-threads=1`
- `git diff --check`

## Diff summary

- Code/content commit: `be4af7c9d1`
- Summary artefact commit: omitted intentionally; reintegration receipt is the source for the final landed squash SHA.
