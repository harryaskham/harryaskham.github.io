# Session summary — bd-52ac40 peer-prober data-plane cross-check

## Goal

Stop the peer probe loop from flipping a peer to `Unreachable` when
its `/api/v1/config/hash` endpoint stalls but its data-plane API
(`/api/v1/agents/summary`) keeps responding. Operators verified the
exact pattern on `ms-dev` and `pocket4`: TCP open, `bd list` < 2s,
`agents/summary` < 2s, `/config/hash` > 15s — yet the operator
surface showed those nodes as Unreachable for 30+ minutes, inflating
the stranded-agent count.

## Bead(s)

- `bd-52ac40` — Peer prober false-Unreachable persists >30min despite
  healthy data plane (bd-84d22a regression)

## Before state

- Failing tests: none (the bug is operator-visible, not test-visible
  — the existing prober tests cover threshold + reset behaviour but
  none crossed the config-probe vs API-probe split).
- Relevant metrics: in `crates/caco-daemon/src/replication.rs`,
  `peer_probe_loop` ran `probe_peer_api_roundtrip` only when
  `config_result.is_ok()`. When `/config/hash` failed, the probe
  loop incremented `consecutive_probe_failures` and — once it hit
  `PEER_PROBE_UNREACHABLE_THRESHOLD` (2) — flipped
  `peer.status = Unreachable`. That happened even when the
  data-plane was responsive every cycle, because the data-plane
  was never probed in the failure path.
- Context: bd-84d22a (5s → 10s timeout + in-cycle retry + 2-cycle
  threshold) addressed transient TLS jitter but did not address
  persistent hash-endpoint slowness with healthy data plane.

## After state

- Failing tests: none. `cargo test -p caco-daemon --lib -- failure_decision data_plane unreachable`
  reports 16 passed (3 new). `cargo test-small` reports 184 passed.
- Relevant metrics: `peer_probe_loop` now ALWAYS runs
  `probe_peer_api_roundtrip` regardless of the config-probe result.
  In the config-probe failure branch, the new logic checks
  `matches!(api_result, Some(Ok(())))`:
    - If the data-plane probe succeeded, mark `api_reachable = true`,
      clear `api_last_failure`, and reset
      `consecutive_probe_failures = 0`. `peer.status` is left at its
      previous value (preserves Match / Mismatch / Reachable). The
      hash failure is still recorded in `peer.last_failure` for
      diagnostics so operators can see "the hash endpoint is slow
      but the node is responsive".
    - If the data-plane probe ALSO failed (or returned an error), the
      bd-84d22a threshold logic applies as before: increment counter,
      record both failures, and promote to Unreachable once the
      threshold is reached.
- Context: bd-52ac40 ACs satisfied. The "false Unreachable >30min"
  pattern stops occurring because the data-plane probe success
  resets the counter every cycle, breaking the runaway accumulation.
  Recovery from a previously-Unreachable state via data-plane-only
  responsiveness is also supported (counter resets to 0; status
  stays Unreachable until the next full Match / Mismatch / Reachable
  config-hash success — which is the right separation of concerns,
  matching the bead's "separate probe-unreachable from data-plane-
  unreachable" suggestion as a partial implementation: we don't
  expose two separate fields yet, but the data-plane signal now
  blocks the false promotion that was the operator-visible harm).

## Diff summary

- Files touched:
  - `crates/caco-daemon/src/replication.rs`:
    - `peer_probe_loop`: API roundtrip probe now always runs (one-line
      structural change inside the `let api_result = ...` block;
      preserved `Option<Result<...>>` shape so the success branch is
      byte-identical).
    - Failure-handling branch in `match config_result`: new `api_ok`
      check splits into two paths (data-plane-alive vs dual-failure)
      with explicit comments tying back to bd-52ac40 and the
      operator-visible failure mode.
    - Test helper `apply_failure_decision_with_api(peer, failure,
      api_ok)` introduced; old `apply_failure_decision(peer, failure)`
      kept as a thin wrapper that defers to the new helper with
      `api_ok = false` so the existing 4 tests are byte-compatible
      and the 2-arg signature still compiles.
    - 3 new unit tests:
      `data_plane_api_success_prevents_unreachable_promotion`,
      `dual_probe_failure_still_promotes_to_unreachable`,
      `data_plane_recovery_resets_unreachable_promotion_clock`.
- Tests: +3 unit tests, no fixtures changed, no API changes.

## Embedded artefacts

(None — pure daemon logic change.)

## Operator-takeaway

`ms-dev` and `pocket4` should stop flipping Unreachable on the
operator surface within one probe cycle of this landing on their
respective node-versions. The hash failure still surfaces in
`peer.last_failure` for diagnostics — operators who want to chase
the slow hash-endpoint root cause can grep for transport / http
phase failures with `last_failure` set while `api_reachable: true`.
A natural follow-up bead (not filed): expose the split as two
separate operator-visible fields ("probe healthy" vs "data plane
healthy") so the dashboard can render a yellow/amber state instead
of just hiding the hash-endpoint slowness behind data-plane
responsiveness.
