# Session summary — caco doctor schema drift detection (bd-9f1dd4)

## Goal

bd-f3968d shipped read-only `caco daemon schema-info`. This
bead adds the drift-detection layer originally requested in
bd-2a2f96: `caco doctor schema` compares live
`pragma_table_info` against a binary-baked expected baseline
and reports MISSING / EXTRA columns per table.

## Bead(s)

- `bd-9f1dd4` — [bd-f3968d follow-up] caco doctor schema:
  drift detection (P3 feature)

## Before state

- `caco daemon schema-info` (bd-f3968d) prints the live
  schema but doesn't compare it against anything.
- No way for operators to detect "binary v1.2.489 expects
  column X but live DB v1.2.460 doesn't have it" before
  the failing query surfaces in user-facing logs.

## After state

- New CLI: `caco doctor schema [--db daemon|beads|all] [--json]`
- Hand-maintained baseline of expected columns per tracked
  table (8 daemon tables + 1 beads table; all the most-
  frequently-evolving ones from this session's bd-2a2f96
  audit).
- Per-table drift report:
  - `OK (N columns)` when matched
  - `MISSING in live (binary expects): col1, col2`
  - `EXTRA in live (binary doesn't expect): col3`
  - `CRITICAL: table missing from live DB` when the table
    itself is gone
- Text mode returns CliError on drift (so cron/CI fails
  loudly); --json mode always returns Ok with the structured
  drift body (so tooling decides policy).
- 3 unit tests: bogus filter rejected, missing-DB handled
  cleanly in --json, drift detection for both MISSING and
  EXTRA columns.

## Diff summary

- Files touched (+~270 / −1):
  - `crates/caco-cli/src/lib.rs`:
    - CommandSpec `doctor` gains a `schema` subcommand leaf.
    - New `[cmd, sub] if cmd == "doctor" && sub == "schema"`
      dispatch arm wraps `dispatch_doctor_schema` in `Outcome`.
    - `expected_schema_baseline()`: hand-maintained
      `BTreeMap<&str, Vec<(&str, Vec<&str>)>>` mapping db
      label → tables → expected columns.
    - `dispatch_doctor_schema`: ~110 LOC. Opens DB, walks
      baseline, runs `PRAGMA table_info(<table>)` per table,
      computes set-difference per direction, formats text
      and JSON outputs.
    - 3 unit tests in `tests` module.

### Drive-by

- `crates/caco-cli/src/lib.rs:79706`: backfilled
  `on_revival: None` on the Profile literal (broken-on-main
  wave #11 from peer's bd-a1ec44 landing).

## Verification

- `cargo build -p caco-cli`: clean.
- `cargo test -p caco-cli --lib doctor_schema`: 3 pass.
- `cargo test-small`: 56 pass.
- `cargo clippy -p caco-cli --lib --tests -- -D warnings`:
  clean.

## Operator-takeaway

Sample drift detection on a v1.2.460 worker against a
v1.2.489 binary baseline:

```
$ caco doctor schema
db: daemon (~/.cacophony/daemon/daemon.db)
  table: feed_events
    OK (8 columns)
  table: project_messages
    MISSING in live (binary expects): visibility, delivered_at

summary: 2 drift column(s), 0 missing table(s)
```

Exit code 1 on drift (text mode); --json always exit 0
with structured body for tooling.

## Building blocks complete

This closes the third building block from bd-2a2f96
(daemon DB schema migration story). All three follow-ups
shipped this session:

1. **bd-8f0b5b**: pragma-probe gate replaces swallowed
   ALTER errors in dynamic_registry.
2. **bd-bd2545**: per-site `eprintln` on actual ALTER paths
   in messaging / dynamic_registry / beads.
3. **bd-9f1dd4** (this slice): doctor-schema drift detection.

Together: operators can now (a) trust migrations don't
silently fail, (b) see migrations happen in stderr, (c)
detect schema/binary drift before it bites.

## Maintenance note

`expected_schema_baseline()` is hand-maintained today.
Future iterations could codegen from canonical
`CREATE TABLE` strings via a build.rs step. For now, the
drift detector ships as-is and operators get value
immediately; baseline maintenance is a small per-PR
checklist item.
