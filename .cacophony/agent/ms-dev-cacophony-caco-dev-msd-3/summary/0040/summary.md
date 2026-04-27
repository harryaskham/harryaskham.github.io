# Session summary — transient rebase index-lock recovery guidance

## Goal

Improve the first-party `caco agent rebase` recovery path after a transient Git `index.lock` failure interrupts rebase replay. The goal was to distinguish this recoverable shape from a real merge conflict so agents get the right next command instead of confusing “blocked by merge conflict” output with no conflicting files.

## Bead(s)

- `bd-7f9050` — Improve caco agent rebase recovery after transient index.lock failures

## Before state

- Failing tests: `cargo test-small` currently fails on the unrelated broken-on-main `supervisor_config_tests::enterprise_theme_is_registered_and_well_formed`, which is owned by `ctkj3u4xdgddgquf` under `bd-ddcb2a`.
- Relevant metrics: focused `cargo test -p caco-cli bd_7f9050 -- --nocapture` passed; `cargo clippy -p caco-cli --all-targets -- -D warnings` passed; `git diff --check` passed.
- Context: `dispatch_agent_rebase` treated all nonzero `git rebase` exits as merge conflicts. A transient `.git/index.lock` failure could leave no lock file, no dirty worktree, and a rebase state ready for `git rebase --continue`, but the CLI still told the operator to resolve merge conflicts.

## After state

- Failing tests: unrelated `bd-ddcb2a` remains broken-on-main during this session and is owned by another worker; no `bd-7f9050` focused failures remain.
- Relevant metrics: +3 focused caco-cli unit tests for the transient lock classifier/message; caco-cli clippy passed.
- Context: the rebase failure path now classifies a stale index-lock interruption only when Git mentions `index.lock`, the lock no longer exists, rebase state is present, the worktree is clean, and no unmerged files are captured. That case emits recovery guidance to inspect status and run `GIT_EDITOR=true git rebase --continue`; real conflicts and still-present locks keep the merge-conflict path.

## Diff summary

- Commits: `a88dada08` (`bd-7f9050: clarify transient rebase index lock recovery`)
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: +3 / -0 / flipped 0
- Behavioural delta: `caco agent rebase` now surfaces a distinct `transient_index_lock` JSON failure kind and a human-readable transient-index-lock recovery message instead of mislabeling the clean rebase-continue state as a merge conflict.

## Operator-takeaway

When Git briefly trips over `.git/index.lock` during rebase replay, agents should now get the precise safe recovery step rather than wasting time looking for nonexistent conflict files.
