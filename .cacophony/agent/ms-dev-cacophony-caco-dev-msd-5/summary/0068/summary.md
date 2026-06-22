# Session summary — bd-238c3d speculative merge artifact cleanup

## Goal

Address `bd-238c3d`: delete selected speculative merge artifact directories after the retention plan exists. Reporting and queue-runner execution remain out of scope.

## Changes

- Added `cleanup_speculative_merge_artifacts(...)` to execute a retention plan by deleting only artifact directories listed in `plan.evict`.
- Added cleanup report/result structs with requested/removed counts, path, per-artifact removed flag, and reason.
- Cleanup constrains artifact ids to a single path component under the provided root to avoid path traversal/absolute-path deletion.
- Missing artifact directories are treated as idempotent no-ops.
- Non-directory paths and unsafe ids return errors.
- Added regressions for directory removal/idempotency and unsafe id rejection.

## Validation

- `cargo test -p caco-daemon --lib cleanup_speculative_merge_artifacts -- --test-threads=1`
- `git diff --check`

## Diff summary

- Code/content commit: `cafeb4457a`
- Summary artefact commit: omitted intentionally; reintegration receipt is the source for the final landed squash SHA.
