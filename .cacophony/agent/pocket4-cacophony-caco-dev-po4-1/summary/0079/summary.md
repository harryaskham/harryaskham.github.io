# Session summary — Remove client-side board visibility preflight from beads CRUD

## Goal

Fix P0 bd-3ae61c: the CLI leaked internal git sync state to agents by
running a preflight check before bead mutations, causing persistent
blocking during background sync windows.

## Bead(s)

- `bd-3ae61c` — Simplify beads CRUD API so git sync state never leaks to agents

## Before state

- Failing tests: none
- `caco bd claim`, `caco bd unclaim` called `bd_board_mutation_preflight()` which
  queried `/api/v1/beads/status` and rejected mutations when `sync_in_progress=true`
  or `ahead>0` or `behind>0`, even though the daemon CRUD handlers use SQLite
  directly and never hold the git sync lock.
- Multiple agents (po4-1, po4-2, msm-2) blocked for hours on routine close/claim
  operations during background sync windows.

## After state

- Failing tests: none
- `bd_board_mutation_preflight()` and `bd_board_status_allows_mutation()` removed
  entirely along with 4 unit tests.
- Common instructions now include "Trust the CLI surface for bead operations"
  guidance telling agents not to invent client-side sync-state polling.
- Plugin agent wrappers regenerated.

## Diff summary

- Commits: 01eb87e60
- Files touched: `crates/caco-cli/src/lib.rs` (-282/+7), `crates/caco-profile/src/common_instructions.txt`,
  `crates/caco-profile/src/instructions.rs`, 5 plugin wrappers
- Tests: -4 (removed preflight tests), +1 (trust-CLI assertion)
- Behavioural delta: `caco bd claim` and `caco bd unclaim` no longer query
  beads/status before mutating; they go straight to the daemon API.

## Operator-takeaway

The entire board-visibility preflight was client-side over-regulation. The daemon
CRUD handlers never competed with the git sync lock — they use the SQLite store
directly. Removing the preflight eliminates the class of "sync_in_progress blocks
routine CRUD" failures that plagued multiple agents for days.
