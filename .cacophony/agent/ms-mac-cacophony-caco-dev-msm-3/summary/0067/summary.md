# Session summary 0067 — bd-2dae3c bd triage validators

## Goal

Close the silent-accept and empty-string bypass holes in caco bd
triage's --type / --limit / --label / --creator filters.

## Bead(s)

- bd-2dae3c — bd triage silent-accept + empty bypass (issues 6, 7, 8)

## Before state

- --type bogus, --limit bogus silently dropped + first bead returned.
- --type/--label/--creator/--limit '' bypass (4 of the 22-surface
  empty-string-bypass cohort).

## After state

- --type validates against ALLOWED_BEAD_TYPE (bug/task/feature) with
  inline-allowed-values phrasing.
- --limit gets a type-aware validator complementary to the existing
  1-50 range check.
- All four flags reject empty strings up-front with explicit
  remediation phrasing.

## Diff summary

- Commit: ebc46b7bba31
- File: crates/caco-cli/src/lib.rs
- Tests: +1 source-level guard

## Operator-takeaway

Try: caco bd triage --next --type bogus
Or:  caco bd triage --next --label ""
Both now error gold-standard instead of silently filtering.
