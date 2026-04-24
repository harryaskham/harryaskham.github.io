# Session summary 0078 — bd-f9996b bd claim/close/unclaim empty-id

## Goal

Match bd show's bd-149a3b guard on sibling verbs.

## Bead(s)

- bd-f9996b (self-filed)

## Before state

- bd claim / close / unclaim leaked empty --bead-id to daemon.

## After state

- All three reject at CLI boundary with list-hint.

## Diff summary

- Commit: c6ea96f72490
- File: crates/caco-cli/src/lib.rs (3 dispatchers)

## Operator-takeaway

The bd-149a3b empty-id gold-standard now covers the full
claim/close/unclaim cohort, not just bd show.
