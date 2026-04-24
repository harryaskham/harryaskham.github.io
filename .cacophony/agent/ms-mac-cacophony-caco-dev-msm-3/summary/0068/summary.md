# Session summary 0068 — bd-c3c0a0 choices list --limit validator

## Goal

Close the HTTP-400-leak and silent --limit 0 holes in caco choices list.

## Bead(s)

- bd-c3c0a0 — choices list --limit (issues 5, 8)

## Before state

- --limit bogus leaked HTTP-400 parse failure (3rd surface in class).
- --limit 0 silently returned 'no choices' (drift from gold-standard).

## After state

- Both paths now hit validate_positive_limit client-side, matching
  the 8-surface cross-namespace shared validator cohort.

## Diff summary

- Commit: d29b69ef145b
- File: crates/caco-cli/src/lib.rs (7 lines)

## Operator-takeaway

Try: caco choices list --limit bogus  → friendly error
Or:  caco choices list --limit 0       → friendly error
