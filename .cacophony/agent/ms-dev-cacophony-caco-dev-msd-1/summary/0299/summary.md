# Session summary — bd-c4665b runner.lock recycled-PID reap-gap fix

## Goal

Fix the root cause of the recurring dead-PID merge-queue `runner.lock` that the
controller kept clearing by hand (a stale lock that wedged fleet reintegration —
the same lock-toil class that contributed to today's reintegration storm), plus
the two flaky merge-queue reap tests. The liveness predicate used a bare
pid-existence check (`ps -p`), so an OS-recycled PID looked "alive" and the dead
lock was never auto-reaped; under heavy load `ps` fork-failures were also
mis-read as "dead", wrongly reaping live holders and flaking the tests. Make the
check a true same-process check.

## Bead(s)

- `bd-c4665b` — [reap-gap] runner.lock liveness uses `ps -p` existence (no
  same-process check) -> PID-recycled dead holders never reaped + load
  fail-open flakes tests.

## Before state

- `runner_lock_holder_alive(pid)` ran only `ps -p <pid>` with `.unwrap_or(false)`
  — a pid-EXISTENCE check. Two failure modes, one root: (1) PID recycling made a
  dead holder look alive → stale lock never auto-reaped (manual cancel-stale-lock
  toil); (2) `ps` fork-failure under load → treated as dead → live holders
  wrongly reaped → 2 flaky tests
  (`cancel_stale_runner_lock_refuses_alive_holder_bd_e65fca`,
  `sweep_stale_runner_locks_reaps_dead_skips_alive_and_fresh_bd_e7ff85`).
- Those two tests modeled an "alive holder" as a live PID + `acquired_at` 2h in
  the past — which is exactly the recycled-PID case under correct semantics.

## After state

- `runner_lock_holder_alive(pid, acquired_at)` is now a same-process check:
  - Linux: definitive existence via `/proc/<pid>` (no fork). A process whose
    start time (from `/proc/<pid>/stat` field 22 + `/proc/stat` btime) is AFTER
    `acquired_at` (beyond a 3s tolerance) is a recycled PID → treated as DEAD →
    reaped. A genuine holder (start <= acquired_at) stays alive.
  - Fail-safe: indeterminate start time / no acquired_at / non-Linux falls back
    to existence-only and treats a probe that FAILS TO RUN as ALIVE, so a live
    runner is never reaped out from under itself (removes the fork-fail-to-dead
    flake).
- Tests: all 6 runner-lock tests pass, including a new Linux-gated recycled-PID
  regression test (`cancel_stale_runner_lock_reaps_recycled_pid_holder_bd_c4665b`:
  live `sleep` child + old acquired_at → treated dead → reaped). The two
  previously-flaky tests now model a genuine same-process holder (recent
  acquired_at) and pass deterministically.
- `cargo test -p caco-daemon --lib runner_lock` = 6 passed; `cargo clippy
  -p caco-daemon --tests` introduced no new lints in the touched code.

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-daemon/src/merge_queue.rs` (single file, +139/-13).
  - New `process_start_epoch(pid)` Linux `/proc` helper + `RUNNER_LOCK_START_TIME_TOLERANCE_SECS`.
  - `runner_lock_holder_alive` rewritten to a same-process check (existence via
    /proc, recycled-PID-as-dead via start-time vs acquired_at, fail-safe alive).
  - One call site updated to pass `acquired_at`.
  - 2 existing tests updated to recent acquired_at + 1 new recycled-PID test.
- Tests: +1 (recycled-PID regression); 2 updated to deterministic same-process modeling.
- Behavioural delta: recycled-PID dead `runner.lock`s are now auto-reaped
  (acquire-path reap + periodic sweep + `cancel-stale-lock` all benefit, since
  they share `cancel_stale_runner_lock`); live holders are never wrongly reaped
  under load. No API/signature change visible outside the module.

## Operator-takeaway

The merge-queue dead-PID `runner.lock` that had to be cleared by hand (and that
amplifies reintegration wedges like today's) is fixed at the root: liveness is
now a same-process check, so a recycled PID can no longer masquerade as the live
runner and the stale lock self-heals. Linux uses `/proc` start-time vs the lock's
recorded `acquired_at`; non-Linux/indeterminate cases fail safe to "alive" so a
real runner is never reaped. Watch for fewer manual `cancel-stale-lock`
interventions on busy nodes.
