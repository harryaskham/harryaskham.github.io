# Session summary — transient git lock retry handling

## Goal

Implement `bd-a01a3c` so transient Git lock contention during `caco agent rebase`, `caco agent complete`, and `caco agent reintegrate` is retried and classified separately from real merge conflicts. The operator-facing goal was to stop agents being sent into manual conflict recovery when the raw failure is only `.git/index.lock` / `could not detach HEAD` contention.

## Bead(s)

- `bd-a01a3c` — Retry transient git index.lock during reintegration recovery before surfacing conflict guidance

## Before state

- Failing tests: none known for this bead at start.
- Relevant metrics: an earlier queued `caco-cli` agent-rebase focused test lane existed, but the CLI only classified the explicit `caco agent rebase` failure path; lifecycle reintegration still surfaced lock-looking failures through generic reintegration/merge-conflict formatting.
- Context: ms-mac was recovering from the `bd-dcafee` projects-empty/autowipe incident, so I preserved local WIP, avoided `caco agent rebase` per Harry's directive, and used queued validation only.

## After state

- Failing tests: none observed in the targeted lanes.
- Relevant metrics: queued `CARGO_BUILD_JOBS=2 RUST_MIN_STACK=33554432 cargo test -p caco-cli --lib agent_rebase -- --test-threads=2` passed (`tj-8df2e29c`); queued `CARGO_BUILD_JOBS=2 cargo check -p caco-cli --tests` passed (`tj-4169f782`); queued `CARGO_BUILD_JOBS=2 RUST_MIN_STACK=33554432 cargo test -p caco-cli --lib bd_a01a3c -- --test-threads=2` passed (`tj-17d5d833`); `git diff --check` passed locally.
- Context: the CLI now performs bounded retry/backoff for transient lock-shaped failures and formats exhausted lock contention as retryable lock contention rather than content merge conflict guidance.

## Diff summary

- Commits: `727d1fe0e`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: added focused `bd_a01a3c` unit coverage for transient reintegration lock detection, wrapper retry-before-success behavior, real-conflict preservation, and `could not detach HEAD` classification.
- Behavioural delta: `caco agent rebase` retries lock-shaped rebase failures up to five times and reports retry attempts; `caco agent complete` / `reintegrate` wrap reintegration outcomes/errors with the same bounded retry and avoid merge-conflict recovery copy when no conflicting files are captured and the failure is lock-shaped.

## Operator-takeaway

The fix keeps actual merge conflicts on the existing recovery path, but lock contention now gets several automatic chances to clear and, if it still fails, tells the agent it was a transient Git lock problem rather than implying their code conflicted with main.
