# Session summary — expected_schema_baseline completeness test (bd-47cd35)

## Goal

Pin that expected_schema_baseline() matches init_tables().

## Bead(s)

- `bd-47cd35` — Unit test for expected_schema_baseline completeness (P3 task)

## Before state

- Baseline was hand-maintained with no automated check.

## After state

- 1 test: opens DaemonStore (runs init_tables), inits messaging + dynamic_registry tables, then verifies every column in expected_schema_baseline exists in the live DB.

## Diff summary

- Files touched (+75 / −0):
  - `crates/caco-cli/src/lib.rs`: 1 test.

## Verification

- `cargo test -p caco-cli --lib expected_schema_baseline`: 1 pass.

## Operator-takeaway

Catches baseline drift when init_tables gains a column but expected_schema_baseline is not updated. Pure test addition.
