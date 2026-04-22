# Session summary — bd-b174bb per-persistent-id launch governor

## Goal

Bound the two failure-amplification mechanisms the existing
`PersistentSentinel` backoff state machine does not fully cover: two
reconcile ticks racing on the same persistent id, and failure paths
that return `Err` before reaching `mark_failed` and so never advance
backoff at all.

## Bead(s)

- `bd-b174bb` — Per-agent resource accounting: open-fd cap,
  spawn-rate cap, bounded retry concurrency (P1 feature)
- (parent: `bd-07bd29` — non-fatal agent-subprocess errors;
  related: `bd-6bdb17`, `bd-65813b`, `bd-80d6de`, `bd-f49a71`)

## Before state

- Two reconcile ticks could both call `launch_persistent_agent` for
  the same persistent id and both hold tmux/fd resources during
  preflight.
- Pre-`mark_failed` failure paths (config / profile / project lookup)
  returned `Err` without scheduling backoff. `bd-6bdb17` evidence on
  sgu24 showed identical fingerprints firing every ~7 min for over an
  hour with no backoff advancement.
- Failing tests: none.

## After state

- New `caco-daemon::agent_launch_governor` module exposing
  `LaunchGovernor::try_begin_launch(id) -> BeginLaunch`:
  - `Granted(LaunchGuard)` — RAII guard releases the in-flight
    reservation on drop.
  - `AlreadyInFlight` — refuses cleanly; the in-flight launch will
    report its own outcome.
  - `AttemptCeilingReached { attempts, ceiling }` — refuses; lets
    backoff catch up.
- Defaults: 3600 s rolling window, 12 attempts/window per persistent
  id (≈ one every five minutes, matching the bd-07bd29 event-emission
  window).
- Refused attempts (`AlreadyInFlight`) are NOT counted against the
  ceiling — only granted launches consume an attempt slot — so a
  flapping reconcile loop cannot exhaust the ceiling without ever
  doing real work.
- Wired at the entry of `launch_persistent_agent`.
- Refusal returns an `Err` recognised by the existing
  `is_benign_persistent_already_live` classifier (extended to two new
  refusal forms), so the periodic-reconcile arm skips structured
  error reporting for governor refusals — they are not real launch
  failures and must not amplify the very flood the governor exists
  to suppress.
- 7 new unit tests; `cargo test-small` green (45).
- Failing tests: none.

## Diff summary

- Commit: `14c388f0`
- Files touched:
  - `crates/caco-daemon/src/agent_launch_governor.rs` (new, 329 lines incl. tests)
  - `crates/caco-daemon/src/lib.rs` — module registration, `DaemonState`
    field + 14 test-state initializers, governor consultation at
    `launch_persistent_agent` entry, extended benign classifier
- Tests: +7 unit tests (`agent_launch_governor::tests::*`)
- Behavioural delta: a second reconcile tick that races a launch for
  the same persistent id is refused at the entry point (rather than
  proceeding into preflight and racing tmux/fd allocation). Pre-mark_failed
  error paths can no longer fire more than 12 launch attempts per id
  per hour even when they bypass backoff scheduling entirely.

## Operator-takeaway

This closes the third and final supervision-hardening slice of the
bd-07bd29 / bd-6bdb17 line: bd-07bd29 collapsed the event flood,
bd-6bdb17 fixed the misleading exit-code reporting, and bd-b174bb
caps the launch attempt rate at the source so floods stop before
they reach the event pipeline. The remaining acceptance item from
the parent (open-fd cap attributable to a single fingerprint) is
better attacked by ensuring failed launches release tmux + child-pipe
+ lock-file resources cleanly on the failure path; that is a
case-by-case audit rather than a single architectural primitive and
should be tracked as a separate bead if the dispatch storms recur
after the existing three slices are deployed.

## Embedded artefacts

(none)
