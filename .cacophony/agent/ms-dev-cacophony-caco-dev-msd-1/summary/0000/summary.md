# Session summary — bd-b174bb global concurrent-launch cap

## Goal

Close the remaining acceptance item on bd-b174bb (per-agent resource
accounting): bound the open-fd footprint of persistent-agent launches
so a fleet-wide reconcile storm cannot exhaust the daemon's fd table.
Items 1 (per-id concurrency=1) and 3 (attempts/hour ceiling) already
shipped in earlier work on the LaunchGovernor; this session lands
item 2 as a daemon-wide cap.

## Bead(s)

- `bd-b174bb` — Per-agent resource accounting: open-fd cap, spawn-rate
  cap, bounded retry concurrency (parent: `bd-07bd29`).

## Before state

- `LaunchGovernor` exposed only per-persistent-id gating: one in-flight
  launch per id and a 12-attempts-per-hour ceiling per id.
- Audit found every post-tmux-create `return Err` path in
  `lifecycle.rs::create()` already called `kill_tmux_session_on`, so
  per-launch fd release was already correct.
- No global ceiling: an N-agent reconcile tick could fan out to N
  simultaneous launches, each holding tmux session + child pipes +
  bootstrap.log writer + sentinel watch fds.
- 7 governor unit tests; failing tests: none related to this work.

## After state

- New `DEFAULT_MAX_GLOBAL_INFLIGHT = 4` and
  `BeginLaunch::GlobalInflightCapReached { in_flight, cap }` variant.
- `try_begin_launch_at` checks the global cap AFTER the per-id
  AlreadyInFlight check (so per-id is still a fast free refusal) and
  BEFORE per-id attempt accounting (so a globally-refused tick does
  not consume one of the per-id rate-limited slots).
- `launch_persistent_agent` (lib.rs) handles the new variant with a
  quiet skip + structured `Err`.
- 11 governor unit tests, all green. `cargo test-small` workspace-wide
  passes. `cargo clippy -p caco-daemon --lib --tests` clean.

## Diff summary

- Commit: `79b65f33` (bd-b174bb: global concurrent-launch cap for
  open-fd accounting).
- Files touched:
  - `crates/caco-daemon/src/agent_launch_governor.rs` — new variant,
    new constant, `with_global_cap` constructor, `current_global_inflight`
    accessor, 4 new tests.
  - `crates/caco-daemon/src/lib.rs` — handle new variant in
    `launch_persistent_agent`.
- Tests: +4 / -0 / flipped 0.
- Behavioural delta: when ≥4 persistent-agent launches are in flight,
  further launch attempts return a structured `Err` and skip the tick;
  the next reconcile retries once an in-flight launch releases.

## Operator-takeaway

The launch governor is now fully bounded along all three bd-b174bb
axes (per-id concurrency, per-id rate, daemon-wide concurrency), so
a fleet-wide failure mode in persistent-agent launches cannot
cascade into daemon fd exhaustion. The default global cap of 4 was
chosen empirically against the bd-6bdb17 reconcile-storm evidence
(>8 agents reconciling on the same tick); if production telemetry
shows the cap is under-tuned in either direction, it can be lifted
to a config field without changing the surface.
