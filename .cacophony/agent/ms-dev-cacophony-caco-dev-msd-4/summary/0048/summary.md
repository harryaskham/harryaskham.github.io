# Session summary — warmed state-branch summary cache

## Goal

Keep recorded-summary browsing fast by giving the daemon a warmed per-project cacophony-state checkout instead of making interactive TUI/web summary requests depend on cold remote state-branch access.

## Bead(s)

- `bd-2aee57` — Cache project state branches in daemon for quick TUI summary serving

## Before state

- Failing tests: none in targeted validation.
- Relevant metrics: summary requests could only read the cacophony-state branch through each project canonical checkout's refs, falling back to fetches if the state ref was cold.
- Context: `bd-8cb6c4` had already bounded the list hot path, but this follow-up required an explicit daemon-managed state-branch cache under `$CACOPHONY_DIR/state-branches/<project>/`.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: daemon init/refresh/force-refresh paths now warm a per-project state-branch checkout, and summary list/show/raw handlers prefer that warmed checkout before falling back to the canonical project checkout.
- Context: the cache is read-only from summary serving's perspective and remains separate from the editable canonical project checkout.

## Diff summary

- Commits: `8e58eaed9`
- Files touched: `crates/caco-daemon/src/checkout.rs`, `crates/caco-daemon/src/lib.rs`, `SPEC.md`
- Tests: `cargo fmt --all -- --check`; `cargo test -p caco-daemon checkout::tests::init_warms_project_state_branch_checkout --lib`; `cargo test -p caco-daemon summary:: --lib`; `git diff --check`
- Behavioural delta: Cacophony daemons maintain warm cacophony-state branch clones at `$CACOPHONY_DIR/state-branches/<project>/` and summary endpoints prefer those clones for fast TUI/web serving.

## Operator-takeaway

Recorded summary browsing now has a daemon-owned warm-cache path: the TUI should no longer need to discover or fetch the state branch on-demand when rendering summaries.
