# Session summary — bd-5e8c76 detect staged-deleted checkout drift

## Goal

Address `bd-5e8c76`: make `caco agent rebase` detect/report an agent checkout whose recorded branch is missing/wrong and whose index has staged-all-deleted with same-path untracked files.

## Changes

- Added pure staged-deleted/untracked-same-path drift parsing and diagnostic helpers for agent rebase.
- `dispatch_agent_rebase` now checks the current branch, recorded agent branch existence, and porcelain status before fetch/rebase.
- If the checkout is on the wrong branch or the recorded branch is missing while status shows staged deletions plus same-path untracked files, rebase refuses with a structured `bd-5e8c76` diagnostic instead of proceeding into lower-level Git failures.
- Diagnostic includes current branch, expected branch, missing-branch marker, counts, sample paths, and first-party recovery guidance.
- Added regression for the exact drift shape and a non-drift matching-branch case.

## Validation

- `cargo test -p caco-cli --lib agent_rebase_detects_missing_branch_staged_deleted_drift_bd_5e8c76 -- --test-threads=1`
- `git diff --check`

## Diff summary

- Code/content commit: `f2cd2c738c`
- Summary artefact commit: omitted intentionally; reintegration receipt is the source for the final landed squash SHA.
