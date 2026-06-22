# Session summary — bd-912ac9 speculative merge execution

## Goal

Address `bd-912ac9`: run the speculative git merge in an isolated integration checkout after materialization planning exists. Writing artifact files remains out of scope.

## Changes

- Added `SpeculativeMergeIntegrationRunReceipt`.
- Added `run_speculative_merge_integration_checkout(...)`:
  - refuses not-ready integration plans
  - creates the integration checkout parent
  - runs `git worktree add -B <merge_branch> <integration_checkout> <base_ref>`
  - merges planned member branches in order with `git merge --no-edit`
  - records merged branches, failed branch, success/failure reason, and bounded command output
- Added regression that creates a temporary repository with two agent branches, materializes the integration worktree, runs the speculative merges, and verifies both branch files are present in the integration checkout.

## Validation

- `cargo test -p caco-daemon --lib run_speculative_merge_integration_checkout_merges_members_bd_912ac9 -- --test-threads=1`
- `git diff --check`

## Diff summary

- Code/content commit: `57271567ea`
- Summary artefact commit: omitted intentionally; reintegration receipt is the source for the final landed squash SHA.
