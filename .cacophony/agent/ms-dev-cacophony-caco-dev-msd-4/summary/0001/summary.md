# Session summary — bd-ba66f0 persistent log_error per-transition gate

## Goal

Enforce bd-6bdb17's acceptance criterion that `PersistentSentinel`
emit exactly one `log_error` per transition into `Failed`, not one
per retry cycle.

## Bead(s)

- `bd-ba66f0` — PersistentSentinel::mark_failed lacks acceptance-criterion logging: 'one log_error per transition to failed, not one per retry'

## Before state

- Failing tests: bd-c19193 (pre-existing, unrelated).
- The bd-07bd29 `ErrorRateLimiter` deduplicates by fingerprint within a 5-min window. That collapses some flooding but does not enforce the per-transition invariant: small differences in failure detail (different `phase`, slightly different error strings) produce distinct fingerprints and slip past the limiter while the agent remains in the same `Failed` state. Persistent failures retried every reconcile tick (every ~30s) leak through as fresh `log_error` events.
- `mark_failed` had no notion of "transition vs. continuation"; every call bumped counters and stamped `updated_at` regardless of whether the agent was already `Failed`.

## After state

- Failing tests: bd-c19193 (unchanged, pre-existing).
- `PersistentAgentState` gains two fields:
  - `last_failed_transition_at: Option<DateTime<Utc>>` — wall-clock stamp of the most recent non-`Failed` → `Failed` transition.
  - `failed_transition_unreported: bool` — latch armed by `mark_failed` on a fresh transition, consumed by the structured-error reporter.
- `mark_failed` sets both fields *only* when the previous state was not already `Failed`. `Failed` → `Failed` calls leave them untouched.
- `mark_starting`, `mark_running`, and `operator_resume` clear both fields so the next genuine failure re-arms the latch.
- `PersistentSentinel::take_failed_transition_unreported(id, paths) -> bool` consumes the latch (returns `true` exactly once per `Failed` transition; persists the cleared state).
- `PersistentSentinel::had_fresh_failed_transition(id, window)` is a complementary window-based query for callers that prefer time-bounded freshness over the strict latch.
- `lib.rs` periodic-reconcile call site now checks the latch before invoking `report_persistent_launch_failure`. Structured `log_error` fires exactly once per `Failed` transition; subsequent retry-tick failures log to daemon stderr only (the `eprintln!` hint stays in place for debugging).
- Startup-pass and test call sites are intentionally unchanged: a startup pass runs once per daemon boot, so one report per startup-fail is informative rather than noisy, and the test scaffolding wants the report to fire deterministically.

## Diff summary

- Commits: `27c98bcb bd-ba66f0: gate persistent log_error on first-tick-after-Failed transition`
- Files touched:
  - `crates/caco-daemon/src/persistent.rs` — +2 fields on `PersistentAgentState`, latch logic in `mark_failed`, clears in `mark_starting` / `mark_running` / `operator_resume`, two new `PersistentSentinel` methods, doc comments, and 2 new unit tests (~330 lines incl. test boilerplate).
  - `crates/caco-daemon/src/lib.rs` — periodic-reconcile `report_persistent_launch_failure` call wrapped in a `take_failed_transition_unreported` gate (~20 lines).
- Tests: +2 / -0 / flipped 0
  - `mark_failed_arms_unreported_latch_only_on_first_transition` — pins arm-once invariant.
  - `mark_running_clears_failed_transition_latch` — pins clear-and-re-arm invariant.
- Behavioural delta: persistent-agent retry storms no longer multiply structured `log_error` events into the Errors tab. Operators see one structured error per failure transition; the next emission requires the agent to recover and re-fail.

## Operator-takeaway

The bd-07bd29 fingerprint rate-limiter is *complementary* to the new
latch: the limiter still catches floods of identical fingerprints
across different agents (e.g. five agents all hitting the same TLS
handshake bug), and the latch handles the single-agent retry-storm
case the bead called out. Together they should deliver the
"observable failure cadence matches reality, not retry cadence"
behavior bd-6bdb17 wanted. If a future reconcile path needs the
same gate, it should call `take_failed_transition_unreported`
right before its `report_persistent_launch_failure` invocation —
the latch is keyed by `persistent_id`, so distinct agents do not
contend.
