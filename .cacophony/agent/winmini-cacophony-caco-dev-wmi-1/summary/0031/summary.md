# Session summary — bd subsystem wedge guard: skip out-of-range titles in reimport (P1 fix)

## Goal

Multiple agents (this one, helsinki test-user-hel, plus others)
reported caco bd list/show/create/update/sync ALL failing with
"CHECK constraint failed: length(title) >= 1 AND length(title) <= 500".
A single corrupt journal mutation was wedging the entire bd
subsystem because reimport_journal is called on every list/show/sync
and aborts the whole transaction on the first bad row.

## Bead(s)

- Filed via msg (bd create itself was wedged); track as
  bd-3af67d follow-up. Code references "bd-3af67d follow-up".

## Before state

- `index_mutation_in_tx` would attempt to insert any bead from
  the journal regardless of title validity.
- Bad row → SQLite CHECK violation → tx aborted → reimport fails →
  all callers see the same error → entire bd surface unusable.

## After state

- `index_mutation_in_tx` validates title length (1..=500 chars)
  BEFORE attempting the upsert.
- Out-of-range titles: skip with eprintln including mutation id,
  bead id, length, and first 80 chars of the offending title.
- Valid mutations on the same journal stream through unaffected.
- 1 unit test: `reimport_journal_skips_out_of_range_titles` —
  3-line journal (good, bad, good) → both goods imported, bad
  skipped, no error returned.

## Diff summary

- Files touched (+62 / 0):
  - `crates/caco-beads/src/store.rs`: defensive guard at top of
    index_mutation_in_tx + 1 test.

## Verification

- `cargo test -p caco-beads --lib`: 213 pass (was 212).
- `cargo clippy -p caco-beads --lib --tests -- -D warnings`: clean.

## Operator-takeaway

After this lands, a corrupt remote bead can no longer wedge the
entire bd subsystem. The bad row is logged with diagnostic detail
(mutation id, bead id, title preview) so operators can locate
and fix the source. Recovery from the current wedge requires
rolling out this binary; pre-wedge bd state recovers automatically
on next reimport.
