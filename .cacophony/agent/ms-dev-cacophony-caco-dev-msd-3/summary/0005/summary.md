# Session summary — bd-6bdb17: persistent restart-backoff cap escalation to 1h

## Goal

Operator/postmortem context (bd-6bdb17): the sgu24 persistent agent
`node-ctrl-sgu` was hammering `pi install
git:.../agent-utils@v1.0.1` on every reconcile tick (every ~7 min)
indefinitely, polluting the feed and contributing to daemon flap
windows. The bead's acceptance criteria spelt out an exponential
backoff with a hard 1h cap. Most of the surrounding work (per-
fingerprint error rate limiter, per-persistent-id launch governor,
declarative-removed agent pruning, ERR-trap exit-code fix) had
already landed — the remaining gap was that the actual backoff was
still capped at `restart_backoff_secs * 10` = 5 min for the default
30s base, which is so close to the ~7 min reconcile cadence that
chronically failing agents never quiesce. Close that gap.

## Bead(s)

- `bd-6bdb17` — checkout_bootstrap pi-preinstall step doesn't re-run
  for agents with pre-existing dirs — legacy agents stuck in retry
  loop (also: 'exit 0' reported as failure)

Reflection drafts filed this session (no cap, dedup checked):
- `bd-220adc` — make MAX_RESTART_BACKOFF_SECS operator-configurable
  + cap-saturation telemetry
- `bd-ba66f0` — PersistentSentinel::mark_failed should emit one
  log_error per Failed transition, not one per retry tick

## Before state

- `crates/caco-daemon/src/persistent.rs::PersistentSentinel::mark_failed`
  computed `effective_backoff = base * 2^min(restart_failures, 10)`
  then capped at `base * 10`. For the default `restart_backoff_secs`
  of 30s, that's a 300s ceiling.
- Reconcile cadence is ~7 min (~420s) across the fleet, so the cap sat
  inside the cadence: a chronically failing persistent agent would
  re-attempt on every reconcile tick forever.
- Failing tests (relevant subset): none — `auto_restart_exponential_backoff_escalation`
  asserted `delay <= 100` for a 10s base, locking in the old behaviour.

## After state

- New module-level constant
  `crates/caco-daemon/src/persistent.rs::MAX_RESTART_BACKOFF_SECS = 3600`
  documented with bd-6bdb17 rationale.
- `mark_failed` now caps `effective_backoff` at
  `MAX_RESTART_BACKOFF_SECS` regardless of base.
- Existing escalation test extended: drives 20+ failures and asserts
  `delay > 100` (proves we exceeded the old ceiling) plus `delay <=
  MAX_RESTART_BACKOFF_SECS`.
- New test `auto_restart_backoff_cap_exceeds_reconcile_cadence`:
  saturates the cap with `restart_backoff_secs = 30` (the default the
  fleet actually runs) and asserts `delay > 420` — the operationally
  meaningful invariant the bead requires.
- `cargo build -p caco-daemon` clean. `cargo clippy -p caco-daemon`
  clean. `cargo test -p caco-daemon --lib persistent::` clean
  (99/99 + 1 new = 100/100, all green).

## Diff summary

- Commit: `9101d57a`.
- Files touched: `crates/caco-daemon/src/persistent.rs` (+128/-12;
  one new const, one rewritten cap line + doc, one extended
  escalation test, one new cap-vs-cadence test).
- Tests: +1 (`auto_restart_backoff_cap_exceeds_reconcile_cadence`),
  one extended (`auto_restart_exponential_backoff_escalation`).
- Behavioural delta: a persistent agent that fails repeatedly now
  decays its retry rate to one attempt per hour at saturation,
  decoupled from the reconcile cadence. Combined with the
  fingerprint rate limiter and launch-attempt governor that already
  landed, the daemon now has three independent layers preventing
  reconcile-driven retry storms.

## Embedded artefacts

(none)

## Operator-takeaway

The remaining piece of bd-6bdb17 was a single arithmetic ceiling
(`base * 10`) that had a non-obvious operational interaction with
the reconcile cadence: any cap shorter than the cadence guarantees
infinite retries. Replaced it with a hard 1h ceiling and locked
that invariant in with a test that names the cadence explicitly,
so future tuning can't silently regress past the same trap.
