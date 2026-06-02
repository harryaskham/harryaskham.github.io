# Session summary — bd-fff514: vanished-tmux retry-scheduling regression coverage

## Goal

Close the riskiest remaining slice of the persistent-agent liveness epic
(bd-ceb7d9, Child B): prove that a `restart: always` persistent agent whose
managed runtime disappears after being recorded `running` converges to a dead
state with last-observed liveness evidence AND is scheduled for a
policy-appropriate retry, rather than lingering as stale `running` inventory —
without introducing a parallel retry mechanism alongside the existing mature
reconcile path.

## Bead(s)

- `bd-fff514` — Restart=always persistents: schedule retry on absent runtime + vanished-tmux regression test — bd-ceb7d9 slice
- (parent: `bd-ceb7d9` — Reconcile persistent agent status with actual runtime liveness after crashes)

## Before state

- Failing tests: none.
- The end-to-end retry path already existed and is wired in the periodic
  sentinel loop: `collect_runtime_liveness_failures` (non-mutating probe) →
  `check_runtime_liveness` (`mark_failed` on positive death evidence) →
  `reconcile` (Failed + restart policy, after backoff → `start_actions`).
- Existing coverage proved the generic auto-restart cycle and operator-stop
  suppression, but no single named regression tied the bd-ceb7d9 Child-B
  acceptance criteria together for a managed **Pi** agent with explicit
  assertions that (a) the recorded-running runtime converges to dead, (b) the
  agent does not remain in a live state, (c) last-observed liveness evidence is
  preserved, and (d) the retry is scheduled after the grace/backoff window.

## After state

- Failing tests: none.
- Added `vanished_tmux_pi_persistent_converges_and_schedules_retry_bd_fff514`
  (queued lane `cargo test -p caco-daemon --lib ...` passed, exit 0,
  job tj-264fb57a).
- The retry-scheduling-on-absent-runtime acceptance criterion is now backed by
  an explicit regression that exercises the real reconcile path against a Pi
  declaration: recorded `Running` → tmux vanishes → non-mutating probe reports
  evidence → `check_runtime_liveness` converges to `Failed` (is_live() == false,
  operator_stopped == false, last_error carries the vanished session name) →
  reconcile suppresses retry during backoff → reconcile schedules the retry once
  backoff expires.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `crates/caco-daemon/src/persistent.rs` (+122 lines, test-only).
- Tests: +1 (`vanished_tmux_pi_persistent_converges_and_schedules_retry_bd_fff514`).
- Behavioural delta: none to production code — this slice is regression coverage
  that pins the existing restart=always vanished-runtime retry-scheduling
  contract (positive death evidence, no stale running inventory, grace/backoff
  honored) so future refactors of the reconcile/liveness path cannot silently
  regress it.

## Operator-takeaway

The persistent-agent retry-on-vanished-runtime behaviour is real and wired into
the periodic sentinel loop; bd-ceb7d9 Child B was a coverage gap, not a missing
mechanism. This test is the canary: if a future change lets a dead-runtime
persistent linger as `running`, drops the last-observed evidence, or stops
scheduling the restart=always retry, this test fails first. The remaining
bd-ceb7d9 work is the read-only liveness-aware aggregate counts (Child A).
