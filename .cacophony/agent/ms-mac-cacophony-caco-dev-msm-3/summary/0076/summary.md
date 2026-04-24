# Session summary 0076 — bd-5403cf profile/node show empty-name

## Goal

Close the quadruple-quote empty-literal rendering on two more surfaces.

## Bead(s)

- bd-5403cf (self-filed) — profile show / node show --name ''

## Before state

- Both rendered '' as the profile/node name in the not-found error.

## After state

- Both reject up-front via validate_non_empty_id with list-hint.

## Diff summary

- Commit: 169e514c50e9
- File: crates/caco-cli/src/lib.rs

## Operator-takeaway

Empty --name on profile/node show now errors gold-standard with a
pointer to the list command.
