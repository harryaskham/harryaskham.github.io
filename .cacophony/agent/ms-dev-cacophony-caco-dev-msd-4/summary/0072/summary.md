# Session summary — revive stale live-tmux workers

## Goal

Finish the reopened stale-liveness tracker by ensuring that a worker already marked `stale` cannot remain stuck in stale bookkeeping once a later reconcile pass proves its tmux session is still alive. The operator-facing goal was to preserve work and make stale/list/direct status surfaces converge without resetting or reassigning live agents.

## Bead(s)

- `bd-2efa9c` — Active workers remain marked stale with impossible no-tool-activity age

## Before state

- Failing tests: none currently failing for this path, but router health evidence repeatedly showed active workers listed as `stale` even after recent progress or direct-running status.
- Relevant metrics: recurring reports included `no tool activity for >65535s` earlier and later threshold-edge `301–317s` stale markers, with summary/list/direct views disagreeing or stale entries clearing and recurring.
- Context: prior fixes made no-tool-activity advisory for live tmux sessions, refreshed aggregate lists from disk, and invalidated remote snapshot caches, but a stale in-memory row could still remain authoritative after the agent had already received its one-time post-restart liveness refresh.

## After state

- Failing tests: none in the targeted and small-suite validation run.
- Relevant metrics: new regression `reconcile_revives_stalled_live_tmux_after_initial_liveness_refresh_bd_2efa9c` proves a `Stalled` agent with an already-recorded liveness refresh is revived to `Running` when tmux is confirmed alive; `cargo test-small` passed.
- Context: live-tmux proof now always clears stale state/error/blocker and stamps fresh activity for stale snapshots, while ordinary running agents still keep the one-time post-restart refresh guard so prompt-idle workers do not get refreshed forever.

## Diff summary

- Commits: `9f1a79479`
- Files touched: `crates/caco-daemon/src/agent/lifecycle.rs`, `crates/caco-daemon/src/agent/tests.rs`, `SPEC.md`, `README.md`, `AGENTS.md`
- Tests: +1 regression test; existing advisory stale test rerun; `cargo test-small`; `cargo check -p caco-daemon`; `cargo clippy -p caco-daemon`; `cargo fmt --all -- --check`.
- Behavioural delta: stale bookkeeping for tmux-live agents is no longer preserved as authoritative after the post-restart liveness refresh has already run; the reconcile loop revives the row to `running` and leaves destructive recovery gated on positive death evidence.

## Operator-takeaway

This closes the remaining stale false-positive shape from the health-pass evidence: if the daemon can still see the worker's tmux runtime, the worker is treated as alive and status converges back to running instead of inviting duplicate handoff or reset action.
