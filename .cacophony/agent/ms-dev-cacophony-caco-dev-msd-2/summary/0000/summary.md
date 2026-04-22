# Session summary — bd-07bd29 supervision hardening: per-fingerprint error-emission rate-limit

## Goal

Eliminate one of the four candidate mechanisms by which agent-scoped
non-fatal subprocess failures (reconcile / launch / preinstall) could
correlate with daemon flap windows: unbounded floods of identical
`fatal:false` `log_error` events fanning out from N persistent agents
on every retry tick, saturating the event bus, log pipeline, and
cross-node feed.

## Bead(s)

- `bd-07bd29` — Non-fatal agent-subprocess errors must never propagate to
  daemon death — reconcile/launch/preinstall failures are agent-scoped
  (P0 bug)

## Before state

- `report_structured_log_error_best_effort` had no per-fingerprint
  backpressure; every reconcile retry across N agents ran the full
  persist → fan-out → broadcast pipeline.
- Production reconcile/launch paths (`persistent.rs`,
  `agent/lifecycle.rs`, `agent/health.rs`, `agent/mod.rs`,
  `spawn_routing.rs`) audited for `unwrap()`/`expect()` outside
  `#[cfg(test)]`: none found.
- `mark_failed` already implements exponential backoff capped at
  10× base seconds.
- Failing tests: none in scope.

## After state

- New `caco-daemon::error_rate_limit` module: in-memory sliding-window
  limiter keyed by caller-supplied fingerprint; default window 5 min.
- `DaemonState` carries an `Arc<ErrorRateLimiter>`; consulted in
  `report_structured_log_error_best_effort`. Suppressed events are
  collapsed; the next emission after rollover carries a
  `(suppressed N similar in last 300s)` summary suffix and a
  `rate-limited-suppressed:N` label.
- Existing `report_persistent_launch_failure` callers already supply a
  deterministic fingerprint (`persistent-launch:<node>:<id>:<phase>`),
  so the rate-limit applies to the exact flood path called out in the
  bead with no caller changes.
- 7 new unit tests cover: first-emit, dup-within-window suppression,
  independent fingerprints, window rollover with suppressed count,
  1000-event flood collapsing to a single emission, no-fingerprint
  pass-through, and bounded-map pruning.
- Failing tests: none.

## Diff summary

- Commits: `228252d8`
- Files touched:
  - `crates/caco-daemon/src/error_rate_limit.rs` (new, 287 lines incl. tests)
  - `crates/caco-daemon/src/lib.rs` (mod registration + `DaemonState`
    field + 14 test-state initializers + caller integration)
- Tests: +7 unit tests (`error_rate_limit::tests::*`)
- Behavioural delta: best-effort structured log_error emissions
  carrying a fingerprint are now collapsed to one per fingerprint
  per 5 min on this node. Synchronous HTTP `/api/v1/logs/error`
  unaffected; unfingerprinted callers unaffected.

## Operator-takeaway

This is one of four mechanisms bd-07bd29 hypothesised behind tonight's
daemon flap windows; with this in place, a single flapping persistent
agent can no longer fan out hundreds of identical structured error
events per minute. The remaining acceptance items (per-agent open-fd
cap, spawn-rate cap, panic-propagation hardening beyond what the audit
already cleared) are tractable as separate beads — they should land as
focused slices rather than one omnibus PR. Verification of which
mechanism actually fired in production still depends on bd-65813b
(crash-log preservation) landing first.
