# Session summary — agent-list peer snapshot cache freshness

## Goal

This session followed up the reopened `bd-2efa9c` recurrence after the stale-grace patch landed. The specific goal was to explain why `caco agent list --state stale` still showed a remote worker as stale while `caco agent summary` and direct `caco agent status --id` had fresher running state, without restarting workers or touching caco-web.

## Bead(s)

- `bd-2efa9c` — Active workers remain marked stale with impossible no-tool-activity age

## Before state

- Failing tests: none known for this change.
- Relevant metrics: router evidence at 12:48Z showed summary reporting one stale worker (`o99l01hnn52l2547`), canonical stale list reporting two (`07xj6dofld367ecz` and `o99l01hnn52l2547`), and direct status reporting `07xj6dofld367ecz` running.
- Context: the previous fix addressed threshold-edge stale transitions and direct disk refresh, but `/api/v1/agents` had a rendered-response cache keyed only by local lifecycle generation. Remote peer snapshot updates do not bump that local generation.

## After state

- Failing tests: none in focused validation; existing `microvm.rs` warnings remain unrelated.
- Relevant metrics: focused cache invalidation regression tests pass, plus `cargo check -p caco-daemon --tests` passes.
- Context: `/api/v1/agents` now keys its cache on both the local lifecycle generation and a peer daemon-state snapshot fingerprint, so remote rows rebuild when peer snapshots change even if no local agent state changed.

## Diff summary

- Commits: `98116c143`
- Files touched: `crates/caco-daemon/src/agents_list_cache.rs`, `crates/caco-daemon/src/lib.rs`, `SPEC.md`, `README.md`, `docs/daemon.html`
- Tests: added `external_fingerprint_invalidates_cached_entry` and `agents_list_cache_invalidates_when_peer_snapshot_changes`.
- Behavioural delta: `caco agent list` / `/api/v1/agents` no longer keeps serving cached stale remote rows after a peer daemon-state snapshot changes; this targets the observed list-vs-summary-vs-direct mismatch for remote agents.

## Operator-takeaway

The remaining recurrence was a cache freshness bug, not another worker-liveness failure: direct status and summary had fresher data, while the rendered agent-list cache could retain an older peer snapshot. The fix makes peer snapshot changes part of the cache key so aggregate list views converge without worker restarts.
