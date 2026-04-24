# Session summary 0066 — bd-f4957c project show/status validator convergence

## Goal

Converge project show --name and project status --project onto the
shared security-WHY validator already used by project show --project /
build list / changelog show.

## Bead(s)

- bd-f4957c — caco node + project: validator drift (issues 3, 5)

## Before state

- 4 different --project validator conventions across 8 surfaces.
- project show --name bogus and project status --project bogus both
  emitted the truncated 'is not configured' message.

## After state

- All 3 entry paths to project show (positional, --project, --name)
  pass through the same security-WHY validator with the configured
  list inline.
- project status --project does the same.
- Cohort moved from 4 conventions/8 surfaces to 3/8.

## Diff summary

- Commit: 2cac5d3a165f
- File: crates/caco-cli/src/lib.rs
- Tests: +1 source-level guard

## Operator-takeaway

Try: caco project show --name typo
Or:  caco project status --project typo
Both now show the configured project list inline.
