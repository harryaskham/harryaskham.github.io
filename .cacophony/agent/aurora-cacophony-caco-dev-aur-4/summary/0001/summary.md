# Session summary — bd-4e2166: pull-only state sync strands reachable static nodes

## Goal

Fix the root-cause replication bug where a configured, reachable, busy static
node (aurora, and the same class micro-0/astra) silently received zero pull
state replication on a pull-only mesh and never converged — stuck at
liveness=settling, version=-, 0/0 agents, and absent/unreplicated in
`caco agent summary` By Node. The display symptom was already addressed
separately (bd-3e481b); this is the underlying convergence fix.

## Bead(s)

- `bd-4e2166` — Pull-only state sync silently never targets aurora/micro-0/astra:
  reachable static nodes stranded, never converge (By Node gap root cause). P1 bug.

## Before state

- Failing tests: none attributable to this bug; it is a runtime convergence defect.
- `crates/caco-daemon/src/replication.rs`: `pull_from_all_peers` read
  `replicator.peers().await`, the frozen `inner.peers` snapshot computed once at
  `Replicator::new` (daemon startup). Any node failing cluster-addr resolution at
  construction time was silently dropped by `resolve_peers`' `filter_map(...?)`
  and never re-enrolled, even after becoming resolvable/reachable.
- Live evidence: helsinki pulled fine from 6 peers but produced zero
  "incremental pull merged inline state from aurora" lines and zero aurora lines
  in the last 500 daemon.log lines — total silence, the worst failure mode.

## After state

- Failing tests: none. Two new caco-daemon lib tests pass under --test-threads=1
  (validated via daemon queue): `unresolvable_static_peer_is_reported_not_silently_dropped`,
  `unresolvable_static_peers_empty_when_all_resolve`.
- `pull_from_all_peers` re-resolves the live peer set every cycle via
  `resolve_all_peers(self_node, config, &dynamic_entries)` (static config nodes +
  active dynamic/codespace registry entries) instead of the frozen snapshot.
- New pure helper `unresolvable_static_peers()` lets the pull loop log configured
  static nodes that fail cluster-addr resolution this cycle (bounded to the pull
  loop, not the high-frequency forward/dispatch callers) and stop once they resolve.
- `cargo check -p caco-daemon`, `cargo clippy -p caco-daemon --lib` clean (no new
  warnings on replication.rs); the only residual clippy warning is a pre-existing
  unused import in store.rs, untouched here.

## Diff summary

- Code/content commit: d451f76b5768f5ad666e8ff7059c25b8a1988e9d (final landed
  squash SHA will come from the reintegration receipt).
- Files touched: `crates/caco-daemon/src/replication.rs` (+129/-1).
- Tests: +2 (both passing).
- Behavioural delta: a configured, reachable static node can no longer vanish
  from the pull peer set permanently due to a startup-time resolution miss; the
  live loop re-attempts it every cycle and emits a diagnostic while it remains
  unresolvable. Config stays pull-only (push intentionally disabled by operator
  to lighten mesh load); the fix is entirely in pull peer-selection.

## Operator-takeaway

The stranded-node bug was not a firewall or port problem and not the absence of
push — aurora/micro-0/astra were reachable the whole time. It was a frozen
one-time peer snapshot plus a silent `filter_map` drop. The durable lesson:
steady-state replication loops must re-resolve their live peer set each cycle and
must never drop a configured peer without a log line. Watch helsinki's daemon.log
after this lands for new "incremental pull merged inline state from aurora" lines
and the bd-4e2166 skip-warning disappearing as nodes resolve.
