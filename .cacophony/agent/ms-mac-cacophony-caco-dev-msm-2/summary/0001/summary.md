# Session summary — peer-probe false-Unreachable hardening (bd-84d22a)

## Goal

Eliminate the false-Unreachable storms operators were seeing on
ms-mac, where 1-4 peers per probe cycle were getting flagged
Unreachable while every other indicator (TCP probes, mesh-routed bead
list, daemon API roundtrips through the mesh router) confirmed they
were healthy. The rotating-peer pattern across cycles pointed at
transient prober-side state, not actual peer outages.

## Bead(s)

- `bd-84d22a` — agent summary peer-probe falsely flags multiple nodes
  Unreachable while mesh-routed traffic still works (P1, recurring)

## Before state

- `replication::peer_probe_loop` used a 5s per-request timeout against
  a freshly-built `reqwest::Client` per cycle.
- A single failed `/api/v1/config/hash` probe immediately promoted the
  peer to `PeerStatus::Unreachable` and surfaced 19 phantom Stranded
  Agents in the worst-observed sweep (sweep 4, 09:13 BST 2026-04-23).
- No retry layer; no consecutive-failure tracking; cold TLS handshakes
  over Tailscale could exceed the 5s budget on the first attempt and
  flip the operator-visible summary state.

## After state

- New constants in `replication.rs`:
  - `PEER_PROBE_TIMEOUT_SECS = 10` (was 5) — cold TLS-handshake budget.
  - `PEER_PROBE_UNREACHABLE_THRESHOLD = 2` — consecutive failed cycles
    before the peer flips to Unreachable (~30s of confirmed failure).
  - `PEER_PROBE_RETRY_DELAY_MS = 200` — in-cycle one-shot retry breather.
- `peer_probe_loop` now:
  1. Builds the mTLS client with the new 10s timeout.
  2. On first `probe_peer_config_hash` failure, sleeps 200ms and
     retries once before booking a cycle-failure.
  3. Tracks `consecutive_probe_failures` per `PeerReachability`. Below
     the 2-cycle threshold, the peer's `status` and `api_reachable`
     fields keep their prior values so the operator-visible summary
     does NOT flap on a single-cycle TLS jitter; the failure metadata
     is still recorded for diagnostics.
  4. A successful probe resets the counter to zero.
- `PeerReachability` extended with `consecutive_probe_failures: u32`
  (`#[serde(default)]` so older serialized state migrates cleanly).
- 4 PeerReachability constructor sites in `beads.rs`, `lib.rs`,
  `replication.rs` updated to populate the new field.
- 6 new passing unit tests (in `replication::tests`):
  - `first_failure_does_not_promote_peer_to_unreachable`
  - `second_consecutive_failure_promotes_to_unreachable`
  - `intervening_success_resets_failure_counter`
  - `unreachable_threshold_constant_is_at_least_two` (regression pin)
  - `probe_timeout_at_least_doubled_from_original_5s` (regression pin)
  - `probe_cycle_budget_fits_inside_interval` (sanity check on the
    interaction between threshold, timeout, retry, and interval)
- All 148 replication-module tests green; clippy clean.

## Diff summary

- Files touched:
  - `crates/caco-daemon/src/replication.rs` — new constants + struct
    field + retry logic + 6 new tests
  - `crates/caco-daemon/src/lib.rs` — 1 PeerReachability constructor
  - `crates/caco-daemon/src/beads.rs` — 1 PeerReachability constructor
- Tests: +6 / -0 / flipped 0
- Behavioural delta: a peer must be unreachable for ≥30s (2 cycles ×
  15s) before the operator-visible summary surface marks it
  Unreachable. Probe-side TLS jitter alone can no longer promote a
  peer to Unreachable.

## Operator-takeaway

Watch ms-mac's `agent summary` over the next 3 sweep windows
(~45 min). The Unreachable column should stay clean unless a peer is
actually down. If a real outage occurs, the summary will lag the
true outage by up to ~30s — that's the deliberate price for
eliminating phantom-unreachability noise. Phantom Stranded Agents
should drop to zero. If false-Unreachable returns, the next thing to
investigate is whether the *first* probe attempt itself is being
blocked at the TLS layer (handshake stall in reqwest's connection
pool); that would warrant adding a connection-pool warmup or
switching the prober to share the mesh router's already-warm pool.
