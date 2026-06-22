# Session summary — bd-3e1ebb isolated handoff successor worktree plan

## Goal

Address `bd-3e1ebb`: create/plan the isolated handoff successor worktree at resolved source refs after source resolution exists. Dirty diff replay and spawn remain out of scope.

## Changes

- Added `HandoffSuccessorWorktreePlan`.
- Added `plan_handoff_successor_worktree(...)`, a pure materialization plan for a resolved handoff successor checkout source.
- The plan derives deterministic isolated worktree directory, successor branch, source ref/head, and refusal for empty roots.
- Added sanitization for path/branch components.
- Added regression covering resolved-source worktree planning and empty-root refusal.

## Validation

- `cargo test -p caco-daemon --lib plan_handoff_successor_worktree_uses_resolved_source_bd_3e1ebb -- --test-threads=1`
- `git diff --check`

## Diff summary

- Code/content commit: `0eecd82e92`
- Summary artefact commit: omitted intentionally; reintegration receipt is the source for the final landed squash SHA.
