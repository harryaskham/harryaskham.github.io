# Session summary — atomic-or-rollback bead mutation index (bd-b20184)

## Goal

Data-integrity defense-in-depth (bd-bfdf6a dim-2): a panic/error between a bead
mutation-persist and its audit-event-write must NOT leave a persisted mutation
without its audit event (the bd-257184 stray-close class). Highest-stakes
close-path change; ctrl provided a starting-map and explicitly asked for careful
queue validation, no rush.

## Bead

- `bd-b20184` (P1 bug, data-integrity) — atomic-or-rollback close/mutation path.

## Root cause (confirmed, map option A)

All bead mutations (close/claim/unclaim/update/delete) funnel through
`BeadsStore::apply_mutation` → `index_mutation_in_tx_with_mode` (caco-beads/
store.rs). Despite the `_in_tx` name, that function ran the bead UPSERT, the
label/dep/attachment replacement, the audit `events` INSERT, the `mutations`
record, provenance, and lifecycle metrics as a series of SEPARATE
auto-committing `conn.execute` statements with NO wrapping transaction. A
panic/error between the close UPSERT and the audit INSERT left the index
half-applied (closed bead, no audit event).

## Fix (surgical, atomic single-tx — no compensating-rollback)

Wrap the index update in ONE sqlite transaction in `apply_mutation` (the WRITE
path) only: open `db.unchecked_transaction()`, run `index_mutation_in_tx(&tx,
...)`, `tx.commit()`. A failure rolls the whole index update back atomically.

CRITICAL: the fix is in `apply_mutation`, NOT inside `index_mutation_in_tx_with_mode`,
because the reimport/journal-replay path already passes its OWN outer `&tx` into
that function — opening a transaction inside it would be BEGIN-inside-BEGIN and
break reimport. Keeping `index_mutation_in_tx` transaction-agnostic lets it run
inside either the new write-tx or the reimport tx. The append-only journal
(apply_mutation step 1) remains the authoritative record; this keeps the derived
SQLite index all-or-nothing.

## Diff summary

- Code commit: `bd-b20184: atomic single-tx for the bead mutation index update`.
  Final landed squash SHA from the reintegration receipt.
- File: `crates/caco-beads/src/store.rs` (apply_mutation tx wrap + an atomicity
  test).
- Test: `close_index_update_is_atomic_no_close_without_audit_bd_b20184` — drops
  the `events` table so the audit INSERT fails mid-index, then asserts the close
  errors AND the issues row stays `open` (rolled back). Plus the full caco-beads
  lib tests (no-legitimate-close-drop / no regression) via the queue.

## Operator-takeaway

A panic between a bead mutation and its audit event can no longer leave a
closed-without-audit bead — the whole SQLite index update is now one atomic
transaction, covering close/claim/unclaim/update/delete in one shot. The
authoritative journal was already atomic; this fixes the derived index. The
best-effort daemon-side BeadClosed FEED event + a stray-close recovery sweep
remain deferred (separate, per the map).
