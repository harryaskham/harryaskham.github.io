# Session summary — daemon stderr diagnostic routing

## Goal

Keep `daemon-crash.log` focused on real daemon crashes after log-monitor showed routine replication, model-discovery, and cluster diagnostics still being written there even after the earlier HTTP-noise fix.

## Bead(s)

- `bd-e5e022` — Replication and cluster diagnostics still write to daemon-crash.log after HTTP noise fix

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: ms-mac log-monitor observed `daemon-crash.log` grow by 22,647 bytes during a healthy daemon window, with no panic/OOM/fatal lines.
- Context: the main daemon's stderr was wired directly to `daemon-crash.log`, so any `eprintln!` from replication, model discovery, peer-message materialisation, or transient cluster connection handling looked like crash evidence.

## After state

- Failing tests: none known for this bead.
- Relevant metrics: focused sidecar tests now assert routine daemon stderr diagnostics are routed away from `daemon-crash.log`, while panic/fatal lines are preserved there.
- Context: the lifecycle manager pipes the daemon's stderr through a classifier thread; non-crash diagnostics are appended to `daemon.log`, already-logged EventLogger mirrors are suppressed, and explicit fatal/crash-like lines still go to `daemon-crash.log`.

## Diff summary

- Commits: `c97f1d1faf`, `275c699691`
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`
- Tests: added 3 focused sidecar classifier regression tests.
- Behavioural delta: daemon stderr is no longer blindly appended to `daemon-crash.log`; replication/state sync, model-discovery, peer-message materialisation, and transient cluster connection diagnostics are classified as daemon-log diagnostics instead.
- Validation: `tj-edcc6dcd` passed the initial focused `bd_e5e022` sidecar tests; `tj-6500767f` passed the expanded focused tests after adding structured-error/fatal classification coverage; `tj-1d5fd20f` and `tj-e921a266` passed the same focused tests again after rebases.

## Operator-takeaway

The crash log should stop growing from normal cluster chatter: it now preserves crash-like stderr, but routine diagnostics remain in daemon logging surfaces where they belong.
