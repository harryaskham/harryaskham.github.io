# Session summary 0079 — bd-178d81 bd update + build cancel empty-id

## Goal

Close two more empty-id holes surfaced during probe sweep.

## Bead(s)

- bd-178d81 (self-filed)

## Before state

- bd update: serde EOF leak via HTTP 404 on trailing-slash URL.
- build cancel: empty-literal in error message.

## After state

- Both reject at CLI boundary.

## Diff summary

- Commit: aad9d702735b
- File: crates/caco-cli/src/lib.rs

## Operator-takeaway

The validate_non_empty_id retrofit arc continues; empty required
flags now fail fast with a list-hint on bd update and build cancel
too.
