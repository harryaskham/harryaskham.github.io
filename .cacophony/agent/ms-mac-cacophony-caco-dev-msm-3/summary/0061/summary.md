# Session summary 0061 — bd-884378 event log filters

## Goal

Close two operator footguns on caco event log: missing --project
filter and the --type/--kind/--action ergonomic gap.

## Bead(s)

- `bd-884378` — caco event log lacks --project + --type

## Before state

- --project triggered bd-b76723 unrecognised-flag warning + unfiltered output.
- --type triggered the same warning; only --command worked.

## After state

- --project filters by substring against caller (client-side).
- --type accepts the operator-natural flag name as a documented alias.
- Both --command and --type provided → explicit alias-conflict error.

## Diff summary

- Commit: ff5e4ff086f2
- File: crates/caco-cli/src/lib.rs
- Tests: +1 source-level guard

## Operator-takeaway

Try: caco event log --project cacophony --since 1h
Or: caco event log --type 'bd close' --limit 5
