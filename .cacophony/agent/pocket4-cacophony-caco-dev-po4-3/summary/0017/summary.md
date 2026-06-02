# Session Summary — bd-066658 (beads claim TOCTOU / SPEC 18.2 write serialization)

## Goal
Close the residual TOCTOU window in `caco-beads` bead claim (and sibling
read-modify-write mutations) against SPEC 18.2's normative requirement that
"writes [are] serialized per project on the local node."

## The gap (as diagnosed in the bead)
`BeadsStore` mutations implement claim/close/delete/update as a synchronous
read-modify-write:
1. `get_bead()` — acquires `db: Mutex<Connection>`, reads status, **releases** it.
2. Precondition checks (status==Open, not draft/blocked/assigned) — **no lock held**.
3. `apply_mutation()` — re-acquires the db lock and writes the **full bead snapshot**.

Because the db lock is released between read and write, on a multi-threaded
runtime two concurrent claims of the same bead can both observe `status==Open`
before either persists. Since the index write is a full-snapshot
(last-writer-wins) write rather than a conditional `UPDATE ... WHERE
status='open'`, the earlier claimer's assignee is silently overwritten while
**both** calls return `Ok` — double-staffing until reconcile corrects it. The
high-frequency no-ID auto-claim path (`claim_next_ready_excluding`) was already
race-aware for the common case (loser gets `InvalidOperation` and moves on), but
the microscopic get/get/apply/apply interleaving remained.

## Change (Option 1 from the bead — preferred)
Added a store-level write-serialization mutex `write_lock: Mutex<()>` to
`BeadsStore`, held across the **entire** get→check→apply sequence in all six
read-modify-write mutations:
- `claim_bead_with_dispatch_adoption` (claim + queued-dispatch adoption)
- `close_bead_with_opts`
- `delete_bead`
- `update_bead`
- `unclaim_bead`
- `handoff_bead`

This directly satisfies "writes serialized per project on the local node" and
generalizes to all read-modify-write mutations, not just claim.

### bd-a9419e interaction (explicitly verified)
The `write_lock` deliberately does **not** wrap the slow reconcile/JSONL-import
path (`reconcile_with_options`), so the bd-a9419e chunked-reconcile contention
fix is preserved — only the fast in-memory read-check-write mutation sequence is
serialized. The `db` mutex retains its existing role.

### Re-entrancy safety
Audited all internal call sites of the guarded methods: no guarded mutation
calls another guarded mutation while holding `write_lock`
(`remove_closed_bead_archive_records_from_hot_store`, the orphan-sweep loop, and
`claim_next_ready_excluding` all call the guarded methods from *unguarded*
contexts, acquiring/releasing the lock per call). The non-reentrant `Mutex`
therefore cannot self-deadlock.

## Validation (queued, per shared-host policy)
- `cargo test -p caco-beads --lib` → **550 passed, 0 failed** (full suite, no regression).
- New regression test `claim_bead_concurrent_single_winner_bd_066658`: 16
  barrier-synchronized threads claim the same bead with distinct actors; asserts
  exactly one `Ok` winner, all others `InvalidOperation`, and the **persisted**
  assignee matches the single winner (no last-writer overwrite). **Passes.**
- `cargo clippy -p caco-beads --lib -- -D warnings` → **passed, exit 0**.

## SPEC
- Implements SPEC 18.2 Consistency ("writes serialized per project on the local
  node"; "the daemon owns any needed waiting, serialization, batching, or
  backpressure ... before durably applying the mutation").

## Diff
See the reintegration receipt for the landed squash SHA. Code commit on the
agent branch: `5575966da`.

## Bead
bd-066658 — claimed, fixed, validated; to be closed after landing on main.

## Provenance note
Original analysis by aurora-cacophony-caco-dev-aur-1 (crate-by-crate review);
implemented here per the bead's recommended Option 1 design.
