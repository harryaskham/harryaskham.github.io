# Session summary — bd-408b35: ReimportRepairCounts on sync/open path

## Goal

Land step (a)+(b) of the bd-18aa72 plan (per msm-3 split note):
a typed `ReimportRepairCounts` struct surfaced from `reimport_journal`
and `reconcile` so the existing eprintln-only repair signal becomes
consumable by the doctor sensor (child 2/3) and reconciler footer
(child 3/3) without further beads-store edits.

## Bead(s)

- `bd-408b35` — [bd-18aa72 child 1/3] expose ReimportRepairCounts on
  caco-beads sync/open path (P3 task).

## Before state

- The lenient `IndexMode::ReimportTruncate` repair path in
  `index_mutation_in_tx_with_mode` eprintln'd a per-repair diagnostic
  but produced no structured output. Callers (reimport_journal,
  reconcile) returned `Result<(), BeadsError>` or `ReconcileResult`
  with no repair-count surface. The doctor sensor (bd-18aa72 child
  2/3) would have to parse stderr to know whether reimport is silently
  fixing data.

## After state

- New `ReimportRepairCounts` struct in `crates/caco-beads/src/model.rs`
  with `empty_title_repairs` and `oversized_title_repairs` counters,
  `is_zero()` predicate, and `total()` aggregator.
- `ReconcileResult` gains a `reimport_repairs: ReimportRepairCounts`
  field with `#[serde(default, skip_serializing_if = "is_zero")]` so
  clean reconcile envelopes carry no all-zero block (existing JSON
  consumers see no envelope drift).
- `index_mutation_in_tx_with_mode` takes an optional `&mut
  ReimportRepairCounts` and increments the appropriate counter in the
  empty-title and oversized-title repair branches (alongside the
  existing eprintln).
- New `index_mutation_in_tx_lenient_with_counts` variant; legacy
  `index_mutation_in_tx_lenient` passes `None` (no counter overhead).
- `reimport_journal` returns `ReimportRepairCounts` instead of `()`.
- `reconcile_with_options` threads its `result.reimport_repairs`
  through the import phase.

## Diff summary

- 2 files changed, +260 / -18:
  - `crates/caco-beads/src/model.rs` — add `ReimportRepairCounts`
    struct and `reimport_repairs` field on `ReconcileResult`.
  - `crates/caco-beads/src/store.rs` — thread counter through
    `index_mutation_in_tx_with_mode`, add
    `index_mutation_in_tx_lenient_with_counts`, change
    `reimport_journal` return type, wire reconcile import phase,
    and add 3 regression tests.

## Validation

- `cargo test -p caco-beads --lib reimport_journal_returns_zero`: pass.
- `cargo test -p caco-beads --lib reimport_journal_counts`: pass.
- `cargo test -p caco-beads --lib reconcile_surfaces`: pass.
- `cargo check --workspace --tests`: clean.

## Operator-takeaway

This is purely internal plumbing — no CLI, TUI, or wire-format
change. The two remaining children (doctor sensor, reconciler
footer) can now consume `ReimportRepairCounts` directly off the
`ReconcileResult` without re-parsing eprintln. The
`skip_serializing_if = "is_zero"` guard ensures that the 99% case
(clean journal, no repairs) adds zero bytes to the JSON envelope.
