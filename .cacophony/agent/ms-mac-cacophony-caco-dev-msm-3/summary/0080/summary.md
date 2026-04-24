# Session summary 0080 — bd-5a5a7a cron empty-name

## Goal

Standardise cron run / cron show on validate_non_empty_id.

## Bead(s)

- bd-5a5a7a (self-filed)

## Before state

- cron run leaked '' to error.
- cron show had a bespoke message (no list-hint).

## After state

- Both emit the gold-standard with 'caco cron list' hint.

## Diff summary

- Commit: 9304d9caad61
- File: crates/caco-cli/src/lib.rs

## Operator-takeaway

Empty --name on cron commands now points to caco cron list.
