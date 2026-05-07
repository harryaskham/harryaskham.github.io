# Session summary — Beelink mesh reachability probe convergence

## Goal

Investigate and fix recurrent operator-visible Beelink mesh unreachability where the Beelink daemon could be locally running while cluster peer state stayed unreachable or stale after recovery.

## Bead(s)

- `bd-eb988e` — Investigate recurrent Beelink mesh unreachable despite healthy local daemon

## Before state

- Bead evidence showed controllers repeatedly saw `beelink-cacophony-technical-writer` unavailable because Beelink stayed `unreachable` from cluster probes after `caco up` / `caco restart` reported local service health.
- Current bounded checks found Beelink reachable from the cluster as `mismatch`, but remote local `caco status --json` still reported `daemon.reachable=false` even though `caco ps` and socket checks showed `caco-daemon` listening on both `127.0.0.1:11100` and `100.116.137.100:12100`.
- Code inspection found the peer probe loop visited peers serially; sleeping/unreachable peers could each consume multiple 10s timeout budgets before later peers such as Beelink were checked.

## After state

- Peer reachability probes now run concurrently per cycle while preserving the existing per-peer config-hash retry, data-plane cross-check, failure threshold, and first-party health transition semantics.
- Local `caco status` daemon liveness now probes lightweight `/api/v1/config/hash` before the heavier `/api/v1/node`, so a busy node endpoint no longer makes a serving daemon look down.
- The recovery latency for a healthy peer after restart is bounded by one peer's timeout budget rather than multiplied by the number of sleeping or unreachable peers earlier in the fleet.

## Diff summary

- Commits: `714225ebe` (code change; this summary is in the following summary commit)
- Files touched: `crates/caco-daemon/src/replication.rs`, `crates/caco-cli/src/lib.rs`
- Tests: updated the peer probe budget contract for concurrent probing.
- Validation: `cargo fmt --all -- --check` passed; queued job `tj-70cf8fed` passed `cargo test -p caco-daemon probe_cycle_parallel_budget_is_per_peer_not_per_fleet_bd_eb988e --lib && cargo test -p caco-cli status_reachability_falls_back_when_authenticated_node_probe_is_slow_bd_8573ef --lib`.
- Behavioural delta: mesh visibility should recover promptly for Beelink even when other peers are asleep or timing out; local status uses the same cheap health/config endpoint family as the mesh probe before falling back to full node diagnostics.

## Operator-takeaway

The recurrent Beelink symptom was consistent with health accounting lag, not a need for manual process kills: serial peer probing let unrelated sleeping nodes delay Beelink convergence, and local status could mislabel a serving daemon as down when the heavy node endpoint was slow.
