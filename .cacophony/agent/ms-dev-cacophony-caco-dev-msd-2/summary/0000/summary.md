# Session summary — bd-5b77b9 cross-peer presence consult before reconciler removes records

## Goal

Close the fine-grained sibling of the bd-cf99b7 incident: the local
beads SQLite store should never be allowed to silently amplify its own
staleness by erasing JSONL records that the rest of the cluster mesh
still holds. The bd-53f5a7 shrink-cap (already landed) catches gross
truncations (>5% AND >50 records). bd-5b77b9 adds a complementary
fine-grained guard that consults peers before any per-record removal.

## Bead(s)

- `bd-5b77b9` — Cross-peer sync verification before any record removal — query mesh first, root from bd-cf99b7 (P1)
- (parent: `bd-cf99b7` — postmortem RCA for the 2026-04-22 reconciler truncation)
- (companion: `bd-53f5a7` — gross-failure shrink-cap, already landed by msm-3)
- (filed follow-up: `bd-cef230` — daemon-side PeerConsult impl + /beads/has/<id> endpoint + config)

## Before state

- Failing tests: none related to this surface
- Reconciler `BeadsStore::reconcile_with_options` had `allow_shrink: bool` only; no per-bead-ID consult layer
- No PeerConsult abstraction existed; no daemon endpoint for per-bead presence
- Open queue had bd-5b77b9 unclaimed despite being explicitly surfaced by caco-ctrl as high-leverage

## After state

- Failing tests: none introduced
- New module `crates/caco-beads/src/peer_consult.rs` defines:
  - `PeerPresence::{Has, MissingFromAll, MaybePresent}` with `must_preserve()` semantics (Has and MaybePresent both preserve; only MissingFromAll permits removal)
  - `PeerConsult` trait (sync, Send + Sync, takes project + ids, returns map)
  - `NoopPeerConsult` default that returns Has for every ID — maximally conservative, preserves legacy behaviour exactly
  - `test_helpers::ScriptedPeerConsult` for unit tests
- `ReconcileOptions` extended with `peer_consult: Option<Arc<dyn PeerConsult>>`; manual `Debug` impl renders the consult's `description()`
- `ReconcileResult` extended with `peer_preserved: usize` and `peer_consult_summary: Option<String>` so operators and follow-up sensors can see the preservation count per reconcile
- `BeadsStore::compute_peer_consult_preservation` and `splice_preserved_lines_sorted` helpers do the work; reconciler invokes them BEFORE the destructive-shrink check so the post-preservation count is what the shrink-cap evaluates
- 3 new unit tests cover Has / unreachable / missing scenarios, all passing
- 237/237 caco-beads lib tests pass; full workspace builds clean; `cargo test-small` (59 tests) passes

## Diff summary

- Commits: `5ad96ceb` (single squash candidate)
- Files touched:
  - `crates/caco-beads/src/lib.rs` (+module declaration + re-exports)
  - `crates/caco-beads/src/model.rs` (`ReconcileOptions` peer_consult field + manual Debug; `ReconcileResult` peer_preserved + peer_consult_summary fields)
  - `crates/caco-beads/src/peer_consult.rs` (NEW, ~310 lines including tests + docs)
  - `crates/caco-beads/src/store.rs` (helpers + reconciler hook + 3 unit tests)
- Tests: +6 (3 in peer_consult.rs, 3 in store.rs `bd_5b77b9` namespace) / -0
- Behavioural delta: when `opts.peer_consult` is `None` (current default everywhere — daemon, CLI, tests) behaviour is byte-identical to before. When `Some(consult)`, removed-ID candidates are diff'd against existing JSONL and any `Has`/`MaybePresent` ID is spliced back into the new content. The bd-53f5a7 shrink-cap still applies on the post-preservation count.

## Operator-takeaway

This bead is **half** the protection. The trait + reconciler hook is
in place and ready to defend the cluster, but it currently always uses
`NoopPeerConsult` (which preserves everything) because no daemon-side
implementation is wired yet. The follow-up `bd-cef230` wires the actual
HTTP fan-out, the timeout config (`beads.peer_consult_timeout_ms`,
default 5000ms), and the doctor sensor. Until that lands, the runtime
behaviour is unchanged from main — but the abstraction is durable and
the daemon-side impl is a localised follow-up that can be claimed by
any worker. Strictly additive: turning the consult on can only ADD
preservation; it can never remove existing safety.
