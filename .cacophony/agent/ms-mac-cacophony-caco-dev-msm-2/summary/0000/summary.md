# Session summary — live startup timeouts warning-only

## Goal

Remove the daemon-health failure mode where slow-but-live agent startup paths are classified as `failed`. The session focused on preserving attachable tmux panes, surfacing warnings instead of false terminal failures, and keeping the startup/reconcile path aligned with the operator directive for the recent ms-mac up/down churn.

## Bead(s)

- `bd-5e336a` — Remove ALL timeout failures on agent startup; downgrade to warnings; reconcile must be fast + async

## Before state

- Failing tests: none known at session start.
- Relevant metrics: `cargo check -p caco-daemon --tests` had not yet been run for this patch; `cargo test-small` was expected to be a fast preflight but ms-mac was cold/heavy.
- Context: `AgentManager::create` could persist `Failed` and kill tmux when alive/readiness/runtime-launch startup timeouts elapsed, and reconcile could kill old live `Starting` sessions to force later failure/retry.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: `cargo check -p caco-daemon --tests` passed; targeted tests `reconcile_warns_for_stale_starting_with_live_tmux` and `reconcile_transitions_stale_starting_agents_to_failed` passed. `cargo test-small` was attempted but exceeded the 300s worker timeout on a cold macOS compile/test run after compiling into the unit test lane; no test failure was observed before timeout.
- Context: live startup timeouts now preserve tmux and return/persist `Starting` with an explicit `bd-5e336a` warning; old live `Starting` agents in reconcile are warning-only, while genuinely dead tmux sessions still transition through the existing failed/retry path.

## Diff summary

- Commits: `e86eb9e24` (code change); summary committed separately on this branch
- Files touched: `SPEC.md`, `crates/caco-daemon/src/agent/lifecycle.rs`, `crates/caco-daemon/src/agent/tests.rs`, `crates/caco-daemon/src/lib.rs`
- Tests: +1 focused regression test; no tests removed.
- Behavioural delta: startup/readiness/runtime-launch timeout conditions no longer kill live tmux sessions or mark agents failed solely because a timeout elapsed. Persistent launch bookkeeping preserves runtime metadata and records `Starting` when the managed agent is warning-only rather than healthy `Running`.

## Operator-takeaway

The daemon should stop producing false failed-agent churn for slow-but-attachable startup. A live pane is now treated as operator-inspectable progress with a warning, not as a terminal failure; dead panes still fail so genuine crashes remain visible.
