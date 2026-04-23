# Session summary — bd-fd300a slice 1: typed BeadTitle wrapper

## Goal

Introduce the type-system half of the title-validation cleanup the
bead calls for, without touching any call sites yet. Future slices
migrate field by field.

## Bead(s)

- `bd-fd300a` — Beads validation: hoist title-length validation
  from CHECK constraint into a typed BeadTitle wrapper (slice 1
  of ~6 in the migration plan)

## Before state

Title-length validation enforced in three places:
- SQLite CHECK constraint (`store.rs:244`).
- Defensive guard in `index_mutation_in_tx_with_mode`
  (bd-3af67d / bd-f86e8a).
- Friendly error rewrite in `error.rs` (bd-fb832d).

bd-f86e8a had to coordinate all three to ship a single fix.

## After state

New module `crates/caco-beads/src/title.rs` (and `pub mod title;`
in `lib.rs`):
- `TITLE_MIN_CHARS = 1`, `TITLE_MAX_CHARS = 500` constants.
- `BeadTitleError { Empty, TooLong { actual_chars } }` with
  Display vocabulary deliberately matching the existing bd-fb832d
  friendly-error wording so the eventual `error.rs` migration is
  a one-line `From` impl.
- `BeadTitle { text, repaired }` newtype:
  - `try_new(text)` write-path constructor (rejects out-of-range).
  - `repair_for_reimport(text)` reconciler-path constructor
    (truncates at TITLE_MAX_CHARS chars — NOT bytes — preserving
    char boundaries for multi-byte glyphs; marks `repaired=true`).
  - Accessors + `Display` + `AsRef<str>` so a future migration of
    a `String` field to `BeadTitle` is a near no-op for `format!()`
    and function-arg passthrough.

## Diff summary

- `crates/caco-beads/src/title.rs`: new file, 290 lines.
- `crates/caco-beads/src/lib.rs`: `pub mod title;` line.
- `cargo test -p caco-beads --lib title::`: 10/10 pass.
- `cargo check --workspace --tests`: clean.

## Embedded artefacts

(none)

## Operator-takeaway

Slice 1 is the load-bearing primitive for the migration. The
remaining slices in the plan are mechanical:
- Slice 2: `CreateBeadParams.title: String` → `BeadTitle`.
- Slice 3: `BeadMutation::Create { title }` likewise.
- Slice 4: reimport path constructs via `repair_for_reimport`;
  the inline truncation in `index_mutation_in_tx_with_mode`
  collapses to a `debug_assert!` (the type already proves it).
- Slice 5: `error.rs` bd-fb832d rewrite collapses to a
  `From<BeadTitleError>`.
- Slice 6: SQLite CHECK constraint is documented as belt-and-
  braces.

Bead remains in_progress; will unclaim. Char-boundary truncation
test (`repair_for_reimport_truncates_at_char_boundary_not_byte_boundary`)
is the load-bearing one — guarantees we never produce invalid
UTF-8 from a forged-emoji journal entry.
