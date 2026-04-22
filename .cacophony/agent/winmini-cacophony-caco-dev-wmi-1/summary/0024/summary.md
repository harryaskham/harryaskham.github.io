# Session summary — Unit tests for apply_title_contains_filter (bd-6d60b7)

## Goal

bd-f64f70 shipped `--title-contains` without tests (time
pressure from rebase churn). Pin the behavior.

## Bead(s)

- `bd-6d60b7` — Unit test for apply_title_contains_filter
  (P3 task)

## Before state

- `apply_title_contains_filter` untested.

## After state

- 2 tests:
  1. Retains matching beads, adjusts count,
     case-insensitive (UPPER matches lower).
  2. Empty needle matches all (str::contains("") == true).

## Diff summary

- Files touched (+47 / −0):
  - `crates/caco-cli/src/lib.rs`: 2 tests in tests module.

## Verification

- `cargo test -p caco-cli --lib title_contains`: 2 pass.

## Operator-takeaway

Pure test addition pinning bd-f64f70's filter behavior.
No runtime change.
