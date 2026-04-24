# Session summary 0064 — bd-0e3e78 agent get URL leak + exit drift

## Goal

Fix the URL-leak and exit-0 drift in caco agent get; verify Issue 1
(--agent-id silent identity confusion) had already landed.

## Bead(s)

- bd-0e3e78 — caco agent SERIOUS BUG cluster

## Before state

- agent get text mode: leaked /api/v1/agents/<bogus>/field/get URL.
- agent get --json: shape-conformant error envelope but exit 0
  (drifter from gold-standard cohort).
- (Issue 1 already landed via bd-3a6078 --agent-id alias.)

## After state

- Transport error scrubber strips ' for url (...)' parenthetical.
- New helper transport_error_envelope_scrubbed for surfaces routing
  operator input into URL paths.
- --json error path now Err so process exits 1 alongside text.

## Diff summary

- Commit: 8c6c21b16716
- File: crates/caco-cli/src/lib.rs (+ drive-by caco-web main red)
- Tests: +2 unit tests for scrubber

## Operator-takeaway

Try: caco agent get --id bogus --field short_name --json; echo $?
   → exit 1 + scrubbed message
