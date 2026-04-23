# Session summary — retention sweep correctly skips non-terminal agents

## Goal

Fix the broken-on-main `retention_sweep_skips_non_completed_agents` test (P1, bd-578267). The test asserted that a `Running` agent passed into `plan_completed_checkout_retention_sweep` would be skipped, but the planner only filtered by `.pruned`, trusting a comment that said "caller pre-filters by state" — a contract the production caller never honoured.

## Bead(s)

- `bd-578267` — [broken-on-main] retention_sweep_skips_non_completed_agents failing

## Before state

- `cargo test -p caco-daemon --lib retention_sweep_skips_non_completed_agents` failed with `assertion failed: plan.targets.is_empty()` at `crates/caco-daemon/src/lib.rs:43346`.
- `plan_completed_checkout_retention_sweep` only filtered `inventory` by `agent.pruned`. A `Running` (or `Stalled`, `Paused`, `Recovering`, `Retrying`, `Pending`, `Starting`, `Waiting`, `Blocked`) agent with an old `created_at` could be silently scheduled for checkout pruning.
- Production caller `run_completed_checkout_retention_sweep` feeds `scan_agents_dir_all()` which returns every non-`Discarded` agent regardless of state. So the comment "caller pre-filters by state" was actively false.

## After state

- 9/9 `retention_sweep_*` tests pass (previously 8/9).
- `cargo test-small` green (57 passed).
- The planner enforces its own state contract: a `.filter(|agent| agent.state.is_terminal())` short-circuits any non-`Completed`/`Failed`/`Stopped`/`Discarded` agent before it can ever be scheduled for pruning.

## Diff summary

- Commit: 938016960 (rebased onto current main)
- File: `crates/caco-daemon/src/lib.rs` (+16 / −3)
- Single change: extend the existing `.filter(|agent| !agent.pruned)` chain in `plan_completed_checkout_retention_sweep` with a second `.filter(|agent| agent.state.is_terminal())` and update the now-stale "caller pre-filters by state" comment.
- Tests: no new tests needed (the existing broken-on-main test now passes and was already structured to assert exactly this contract).
- Behavioural delta: callers can no longer accidentally schedule live agents' checkouts for retention pruning. Production behaviour unchanged in practice because no live agent has an old enough `ended_at` to trip the planner — but the defense-in-depth removes the latent bug class.

## Operator-takeaway

When a function comment says "caller pre-filters X" and there is exactly one production caller that does not pre-filter, the comment is a bug — either fix the caller or move the filter into the function. In this case the function name (`plan_completed_checkout_retention_sweep`) makes the contract self-evident, so enforcing it inside the planner is the right shape: the type system already telegraphs that only completed agents should be inputs, and the filter makes the type-implied contract real.
