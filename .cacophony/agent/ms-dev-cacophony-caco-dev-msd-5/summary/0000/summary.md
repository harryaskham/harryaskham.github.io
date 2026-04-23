# Session summary — bd-07bd29 panic-audit close-out

## Goal

Close the panic-audit acceptance item on bd-07bd29 (the architectural
rule that agent-subprocess errors must never propagate to daemon
death) by removing the last non-test panic-prone sites in the
agent-launch / maintenance hot path. Earlier sessions (msd-2, msd-3)
had already landed the per-fingerprint error rate-limiter and the
per-persistent-id launch governor; the remaining acceptance gap was
the residual `unreachable!()` and `unwrap()` audit.

## Bead(s)

- `bd-07bd29` — Non-fatal agent-subprocess errors must never propagate
  to daemon death — reconcile/launch/preinstall failures are
  agent-scoped (P0 bug, kept claimed; resource-accounting and crash-log
  acceptance items stay under their dedicated follow-ups).

Related, intentionally not closed by this session:
- `bd-b174bb` — per-agent resource accounting (open follow-up).
- `bd-65813b` — silent daemon crash log preservation (open follow-up).

## Before state

- Two non-test panic-prone sites in `crates/caco-daemon/src/agent/spawn.rs`:
  - `build_resume_init_script`: `_ => unreachable!()` after a match
    over the result of `detect_resume_runtime`. Architecturally
    sound today, but a future runtime added to detection without a
    matching arm here would panic on the agent-launch hot path.
  - `clear_pi_session_history`: `session_files.split_first().unwrap()`
    guarded only by an earlier empty-vec early return. Agent-subprocess
    maintenance paths must not depend on a programmer invariant for
    panic-freedom.
- Failing tests in scope: none.
- Existing infra: `ErrorRateLimiter` (5-min window per fingerprint) +
  `LaunchGovernor` (per-persistent-id concurrency + attempts ceiling)
  already wired into `launch_persistent_agent` and
  `report_structured_log_error_best_effort`.

## After state

- Both sites converted to structured `Err` returns / graceful no-ops.
- Two new regression tests in `crates/caco-daemon/src/agent/tests.rs`:
  - `clear_pi_session_history_no_directory_returns_zero_zero`
  - `clear_pi_session_history_keep_last_does_not_panic_on_single_file`
- `cargo test -p caco-daemon --lib clear_pi_session_history`: 7 passed.
- `cargo test -p caco-daemon --lib build_resume_init_script`: 11 passed.
- `cargo clippy -p caco-daemon --lib --tests`: clean.

## Diff summary

- Commits: `c59d1933`
- Files touched:
  - `crates/caco-daemon/src/agent/spawn.rs` (+22 / -2)
  - `crates/caco-daemon/src/agent/tests.rs` (+38 / -0)
- Tests: +2 / 0 flipped / 0 removed.
- Behavioural delta: agent-launch / pi-session-maintenance code paths
  no longer have any reachable `unwrap` / `unreachable!` on the hot
  path; an unforeseen new runtime now surfaces as a `DaemonError` the
  caller already handles, instead of a daemon-thread panic that would
  rely on `spawn_background_task`'s `catch_unwind` to absorb.

## Operator-takeaway

bd-07bd29's architectural rule — agent-subprocess failures must not
take the daemon down — is now defended at three layers: (1) per-
fingerprint error-emission rate limit, (2) per-persistent-id launch
governor, and (3) zero panic-prone code on the agent-launch hot path.
Resource accounting and crash-log preservation remain genuine
follow-ups (bd-b174bb, bd-65813b) and are the next items to land in
the supervision-hardening sweep.
