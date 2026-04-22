# Session summary — bd-80d6de: multi-pass cleanup of orphan cargo/rustc trees on agent stop

## Goal

Make `caco agent stop` actually terminate the cargo/rustc compile graph
that an agent leaves behind in its checkout, so stopped agents stop
consuming CPU and disk. Today, after a stop, rustc workers reparent to
PID 1 and keep compiling for an hour or more, which is what tipped
helsinki into its build-storm + daemon-death loop.

## Bead(s)

- `bd-80d6de` — caco agent stop leaves orphaned cargo/rustc processes running in agent checkout (P1, bug)

## Before state

- Failing tests: none caused by this work (3 pre-existing `stop_*`
  tests fail when run in parallel due to shared tempdir state — fail
  identically on `main` without my changes).
- `cleanup_checkout_processes` was a single-pass `pgrep -f <checkout>`
  followed by SIGTERM (200ms grace) then SIGKILL. cargo regularly
  outran this because it forks fresh rustc workers between the pgrep
  snapshot and the kill.
- `stop()` discarded the kill count, so operators had no signal that
  the stop had to escalate.
- Concrete production evidence in the bead: two helsinki workers
  still building 1h+ after `caco agent stop`; required manual
  `pgrep -af agents/cacophony/<id> | kill -KILL` to actually clear.

## After state

- Failing tests: same 3 pre-existing parallelism flakes (independent of
  this change). All 4131 `cargo test-small` tests pass. All targeted
  `caco-daemon` `stop_*` and `cleanup_checkout_processes_*` tests pass
  individually.
- `cleanup_checkout_processes` now iterates up to 5 passes, sleeping
  150ms between passes so freshly-spawned rustc workers become visible
  to the next `pgrep` snapshot, and returns the total number of
  processes that received signals.
- `stop()` captures the returned count and appends a forced-cleanup
  suffix to `last_error` so operators can see when escalation was
  needed, e.g. `"... — forcibly killed 17 orphaned process(es) from
  checkout (likely cargo/rustc compile tree)"`.
- Survivors after the pass cap are logged at warning level so
  persistent escapees can be chased.

## Diff summary

- Commits: 2d56160c
- Files touched:
  - `crates/caco-daemon/src/agent/health.rs` — multi-pass loop, return
    type now `usize`, post-cap survivor logging
  - `crates/caco-daemon/src/agent/lifecycle.rs` — `stop()` plumbs
    `forced_kills` count into `last_error`; `set_state()` keeps
    fire-and-forget call via `let _ =`
  - `crates/caco-daemon/src/agent/tests.rs` — three new unit tests
- Tests: +3 / -0 / flipped 0
- Behavioural delta: agent stop now reliably tears down cargo/rustc
  trees rooted in the checkout and tells operators when it had to.

## Operator-takeaway

The helsinki build-storm root cause is now fenced. When `caco agent
stop` finishes, the cargo/rustc tree is verifiably gone (or, if some
process refused to die after 5 SIGTERM/SIGKILL passes, you'll see a
`bd-80d6de: WARNING` line naming the survivors). If a stopped agent's
`last_error` mentions "forcibly killed N orphaned process(es)", that's
diagnostic gold — it means the runtime exited cleanly but compile work
was still running, which is the exact pattern that drove the saturation
incident.
