# Session summary — bd-0c15d2 back off connect-retries to expected_unreachable peers

## Goal

Cut wasted mesh/beads connection-retry cycles and cluster-err log noise to peers
that are config-marked `health_expectations.expected_unreachable: true` during an
intentional multi-day offline window (observed: ~40 connect retries to a
known-offline peer per 200 cluster-err lines). The `expected_unreachable`
marking already suppresses outage-flagging in doctor/ops but did NOT slow the
daemon's connect-retry cadence to those peers.

## Bead(s)

- `bd-0c15d2` — Daemon: back off mesh/beads connect retries to
  expected_unreachable peers (efficient cluster usage).

## Before state

- The daemon already has per-peer adaptive undelivered/connect-retry backoff
  (bd-0338f9: `next_undelivered_backoff_secs`, consecutive-failure tracking,
  circuit-breaking), but it plateaus at `RETRY_MAX_INTERVAL_SECS` (300s) for ALL
  peers regardless of `expected_unreachable`. So 4 intentionally-offline nodes
  kept being retried every ~5min, spamming cluster-err logs and burning
  connection cycles for days.

## After state

- `expected_unreachable` peers now escalate their connect-retry backoff to a
  larger ceiling (`RETRY_MAX_INTERVAL_EXPECTED_UNREACHABLE_SECS` = 1800s / 30min)
  instead of 300s — backing off ~6x more aggressively while still probing every
  ~30min so recovery is detected promptly when the node returns. Reachable peers
  are unchanged (still capped at 300s).
- The expected_unreachable peer-name set is derived once at `Replicator::new`
  from static node config (`health_expectations.expected_unreachable`) and
  stored in `ReplicatorInner`; `mark_undelivered_retry_failure` reads it and
  passes the flag into `next_undelivered_backoff_secs`.
- Additive + low-risk (the bead notes it is "purely a retry-cadence efficiency
  improvement, not a liveness blocker"); reachable-peer behaviour is identical.
- Tests: new unit test
  `next_undelivered_backoff_expected_unreachable_uses_larger_ceiling_bd_0c15d2`
  (reachable plateaus at 300s; expected_unreachable escalates 300->600->1200->1800
  and plateaus). The existing `retry_undelivered_applies_per_peer_backoff_bd_0338f9`
  still passes. `cargo test -p caco-daemon --lib` (both) pass on a real compile;
  clippy clean.

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-daemon/src/replication.rs` (single file, +72/-4).
  - New `RETRY_MAX_INTERVAL_EXPECTED_UNREACHABLE_SECS` const.
  - `next_undelivered_backoff_secs` gains an `expected_unreachable` param (larger
    ceiling when set).
  - `ReplicatorInner.expected_unreachable_peers: HashSet<String>` derived in
    `new()` from config; `empty()` updated; `mark_undelivered_retry_failure`
    threads the flag.
  - +1 unit test.
- Tests: +1 (backoff ceiling escalation). Behavioural delta: connect-retry
  backoff to expected_unreachable peers stretches to 30min; reachable peers
  unchanged.

## Operator-takeaway

Intentionally-offline nodes (config `expected_unreachable: true`) no longer get
hammered with ~5-min connect retries — the daemon now backs them off to ~30-min
intervals, cutting cluster-err log noise and wasted connection cycles during
multi-day offline windows, while still probing often enough to detect recovery.
Reachable-peer retry behaviour is untouched.
