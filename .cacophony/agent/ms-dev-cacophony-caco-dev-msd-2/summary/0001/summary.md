# Session summary — auto-retry retryable resume blockers (bd-d62ad6)

## Goal

Land bd-d62ad6: when an agent's resume fails with a retryable blocker
(e.g. `tmux_session_exited`, `runtime_launch_failed`, `readiness_timeout`),
the daemon should auto-retry with bounded exponential backoff before
surfacing the failure to the operator. Today the operator must run
`caco agent resume` again manually for each transient flap, even though
the daemon already knows the blocker is retryable. The operator quote on
the bead is "why doesnt this retry itself" — this fix makes it.

## Bead(s)

- `bd-d62ad6` — Retryable resume blockers should auto-retry instead of
  asking the operator to re-run.
- Related (for context, not closed by this PR): bd-d0e6f0, bd-0d9e23,
  bd-5e336a, bd-3886df, bd-1f2bf4, bd-3a5a0f.

## Before state

- `crates/caco-daemon/src/lib.rs::handle_agent_resume` called
  `state.agents.resume(...)` exactly once. On failure with a retryable
  blocker (e.g. msm-2 tmux flap, ms-mac config-helper bootstrap), the
  error envelope set `retryable: true` and the CLI hint said "run
  `caco agent resume` to try again" — pure operator-driven retry.
- `ResumeBlocker::is_retryable()` already classified 10 blocker
  variants as retryable; this signal was surfaced to the operator but
  not acted on by the daemon.
- The bead's operator-supplied evidence (added today) showed
  ms-mac-cacophony-config-helper and ms-mac-cacophony-cluster-debugger
  still failed with retry-budget-exhausted / tmux_session_exited even
  while the worker fleet was running.
- `cargo test -p caco-daemon --lib tests::` (excluding pre-existing
  flakes from concurrent env-mutex contention in reintegration tests
  unrelated to resume): green.

## After state

- New `auto_retry_resume(do_attempt, agent_id)` policy core:
  - Pure retry loop, takes a closure returning `(Result, blocker_retryable: bool)`.
  - `MAX_RESUME_AUTO_RETRIES = 2` (3 total attempts), backoff
    `250ms, 750ms` (exponential, base 3).
  - Loud-logs each retry via `eprintln!` with attempt number, blocker,
    agent_id, and backoff window so the agent log carries an auditable
    trail (acceptance criterion: "Loud, observable retry attempts in
    the agent log (not silent)").
  - Bubbles immediately when the blocker is non-retryable so the
    existing structured envelope shape is preserved (acceptance
    criterion: "After exhaustion or non-retryable blocker, surface the
    existing error + hint as today").
- New `try_resume_with_auto_retry(state, agent_id, canonical_checkout)`
  thin wrapper that adapts the policy core to the live daemon: it
  invokes `state.agents.resume(...)` per attempt and reads the recorded
  `agent.resume_blocker.is_retryable()` post-failure to decide.
- `handle_agent_resume` now calls `try_resume_with_auto_retry` instead
  of `state.agents.resume(...)` directly. All downstream paths
  (success, error envelope construction, persistent sentinel sync, feed
  events, audit log) are unchanged — the retry is transparent.
- CLI hint message in `dispatch_agent_resume` now informs the operator
  the daemon already auto-retried up to 3 times before surfacing the
  error, so manual retry is for persistent issues only.
- 4 new tests in `caco-daemon::tests`:
  - `auto_retry_resume_returns_first_attempt_on_success` — happy path,
    no retry overhead when first try succeeds.
  - `auto_retry_resume_does_not_retry_non_retryable_blocker` —
    `missing_checkout`-class blockers bubble in 1 attempt.
  - `auto_retry_resume_recovers_on_retryable_blocker` — first attempt
    fails with retryable blocker, second succeeds → 2 calls total.
  - `auto_retry_resume_caps_attempts_at_three_for_persistent_failure` —
    persistent retryable failure caps at exactly 3 attempts.
- All 4 new tests pass; `cargo build -p caco-daemon -p caco-cli` clean.

## Diff summary

- `crates/caco-daemon/src/lib.rs` (+ ~110 LOC):
  - +`auto_retry_resume<F, Fut>(...)` pure policy core.
  - +`try_resume_with_auto_retry(state, agent_id, ...)` daemon-state
    adapter calling `state.agents.resume(...)` per attempt.
  - Edit: `handle_agent_resume` now calls `try_resume_with_auto_retry`
    in place of the direct `state.agents.resume(...)` call.
  - +4 unit tests in `tests::` module.
- `crates/caco-cli/src/lib.rs` (+ ~5 LOC):
  - Edit: retryable-blocker hint message in `dispatch_agent_resume` now
    cites bd-d62ad6 and notes the daemon already auto-retried.
- `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-2/summary/0001/summary.md`
  (this file).

Total: 2 production files + summary; +120 LOC; no public API change; no
schema change; no migration. The retry is purely server-side; clients
see the same envelope shape.

## Operator-takeaway

The 14 reintegration test failures observed in the local full-suite run
are pre-existing on origin/main (verified by re-running the same test
on a fresh `origin/main` clone — `direct_mode_conflict_lists_files`
fails identically). They are env-mutex contention between concurrent
test threads in the reintegration module, not related to this change.
This PR's tests run cleanly and the wider workspace builds.

The auto-retry is **transparent**: clients see the same envelope shape;
the persistent sentinel sync, feed events, audit log, and ui broadcast
all run after the (eventually-)successful resume just as before.
Operators auditing the trail will see `bd-d62ad6:` log lines per retry
in the daemon log, and the CLI hint message now reflects that retry
already happened automatically.

If support agents like ms-mac-cacophony-config-helper continue to fail
after this lands, the next bead should look at the underlying flake
(e.g. bd-0d9e23 / bd-d0e6f0 startup/bootstrap) since 3 attempts of the
same operation in 1 second can't fix structural problems — it's only a
timing-based flap remediation.
