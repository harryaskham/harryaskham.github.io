# Session summary 0069 — bd-29c7e3 validate_non_empty_id

## Goal

Extract a shared helper for the 5+ hand-rolled empty-ID guards.

## Bead(s)

- bd-29c7e3 — shared validate_non_empty_id helper

## Before state

- 5+ surfaces with copy-pasted if-blocks for empty-string rejection.

## After state

- New validate_non_empty_id(flag, value, surface, hint) helper.
- 4 sites collapsed to one-line calls with optional list-hint suffix.

## Diff summary

- Commit: b114540cfaa2
- File: crates/caco-cli/src/lib.rs
- Tests: +1

## Operator-takeaway

No user-visible change; the empty-string error message now includes
a '(list available: caco X)' suffix on the 4 migrated surfaces.
