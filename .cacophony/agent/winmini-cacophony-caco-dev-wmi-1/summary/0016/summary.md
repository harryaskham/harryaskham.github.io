# Session summary — dynamic_registry pragma-probe migration (bd-8f0b5b)

## Goal

bd-2a2f96 audit identified `dynamic_registry::init_table`
as the only ALTER-TABLE site in the codebase that swallowed
errors via `let _ = db.execute(...)` instead of using the
pragma_table_info gate pattern. This bead replaces that
pattern with the explicit gate, mirroring messaging.rs.

## Bead(s)

- `bd-8f0b5b` — [bd-2a2f96 follow-up] dynamic_registry
  init_table: use pragma_table_info gate

## Before state

- 3 ALTER TABLE ADD COLUMN statements running unconditionally
  with errors swallowed.
- Behaviour was idempotent in practice (duplicate-column
  errors discarded), but legitimate failures (locked DB,
  IO error, etc.) were also silently swallowed.

## After state

- Each ADD COLUMN gated by `SELECT COUNT(*) FROM
  pragma_table_info('dynamic_nodes') WHERE name = ?1`.
- Errors now propagate as
  `DaemonError::Other("dynamic_nodes migrate column <c>: <e>")`.
- 2 new unit tests pin behaviour:
  - `init_table_is_idempotent`: 3 sequential calls succeed,
    each migrated column exists exactly once.
  - `init_table_migrates_legacy_schema_missing_columns`:
    bootstrap a legacy-shape table missing the 3 columns,
    run init_table, verify all columns added.

## Diff summary

- Files touched (+76 / −7):
  - `crates/caco-daemon/src/dynamic_registry.rs`: pragma-gated
    migration loop + 2 tests.

## Verification

- `cargo test -p caco-daemon --lib dynamic_registry`: 44 pass
  (was 42; +2 new).
- `cargo test-small`: 56 pass.
- `cargo clippy -p caco-daemon --lib --tests -- -D warnings`:
  clean.

## Operator-takeaway

Pure refactor. No behavioural change for the common path
(fresh DB or already-migrated DB). The change is observable
only when migration would have legitimately failed — those
failures now surface as DaemonError instead of being dropped
on the floor.

This closes one of the three follow-ups identified by
bd-2a2f96. The other two (per-site migration logging,
caco doctor schema probe) remain unfiled pending operator
validation of direction.
