# Session summary — bd-0282ac: bound the peer-probe cycle so one hung dial can't freeze it

## Goal

Stop the daemon's peer-probe loop from wedging fleet-wide under sustained host
load. On a loaded node, one peer's mTLS dial / TLS handshake could hang past the
reqwest per-request timeout; because the cycle awaits the whole `join_all` batch,
that single unbounded future stalled the entire cycle and the loop never
advanced — freezing operator-visible liveness (`caco status` shows nearly all
peers stuck `settling (first probe pending)`, `last_seen` frozen at the last
restart) even though the peers are healthy and serving the cluster API.

## Bead(s)

- `bd-0282ac` — Daemon peer-probe loop wedges fleet-wide under sustained load
  (one hung mTLS dial freezes the join_all batch). P2 bug, daemon/liveness/
  replication. (Server-side companion to the bd-a9419e mTLS-flap symptom.)

## Before state

- `peer_probe_loop` (crates/caco-daemon/src/replication.rs) probed all peers via
  `join_all` each 15s cycle. Per-request reqwest timeout existed
  (`PEER_PROBE_TIMEOUT_SECS = 10`), but no hard OUTER bound per peer future.
- Evidence (ms-mac, 2026-06-01): across 2 restarts + ~9m uptime (~36 expected
  cycles), 0 fresh probes after the first partial cycle, 10/12 peers permanently
  `pending`, including healthy helsinki/aurora/cs-0/1/2. os error 49 had cleared
  yet the loop stayed wedged — proving the loop itself does not recover.

## After state

- Failing tests: none. `replication::tests` = 58 passed / 0 failed. New tests:
  - `peer_probe_timeout_result_marks_timeout_failure_bd_0282ac`
  - `bounded_probe_hard_timeout_unwedges_hung_peer_bd_0282ac`
- Added `PEER_PROBE_HARD_TIMEOUT_SECS = 45` (above the worst-case legitimate
  per-peer budget: config 10s + 200ms retry + config retry 10s + API 10s ~= 30s,
  with margin so a merely-slow-but-progressing peer never false-times-out).
- Extracted `peer_probe_timeout_result(node, secs)` (synthesizes a
  `timeout`-phase `PeerFailure` for config + API) and
  `bounded_probe_peer_for_cycle(client, entry, secs)` (wraps
  `probe_peer_for_cycle` in `tokio::time::timeout`). The `join_all` now maps to
  the bounded helper, so every peer future resolves within the hard bound and
  the batch + loop always advance; a hung peer is recorded unreachable-this-cycle
  and skipped.
- clippy `-p caco-daemon --lib`: clean.

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files touched: crates/caco-daemon/src/replication.rs (+117/-1).
- Tests: +2 (timeout result shape; hard-bound unwedges a never-resolving probe).
- Behavioural delta: per-peer probes are hard-bounded at 45s; under normal
  conditions nothing changes (probes finish in <30s), but a wedged dial can no
  longer stall the cycle or freeze the loop. The reqwest per-request timeout is
  unchanged; this is a strictly-additive outer safety bound.

## Embedded artefacts

- none.

## Operator-takeaway

This is the loop-liveness half of the cluster-probe fragility: the per-REQUEST
timeout was never the gap — the gap was the per-PEER FUTURE being unbounded
inside `join_all`, so one hung dial under load froze every peer's liveness at
once. The hard 45s outer bound guarantees the cycle always completes. It is the
server-side companion to bd-a9419e (helsinki mTLS proxy flaps): the same
under-load hung-dial class that wedged this loop is consistent with the proxy
flap symptom, so this bounding pattern is the right shape there too if that bead
is picked up.
