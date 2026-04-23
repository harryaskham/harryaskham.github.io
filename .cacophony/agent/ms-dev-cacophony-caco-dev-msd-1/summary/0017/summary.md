# Session summary — bd-2c88ed project show --name spec + conflict

## Goal
Make caco project show's three documented spellings (positional,
--project, --name) all behave consistently: no bogus 'ignored'
warning for --name; explicit conflict error when two flag-form
spellings collide.

## Bead(s)
- `bd-2c88ed` (P3 bug, test-user) — project show --name surfaces.

## Before state
- --name worked at runtime via the dispatch fallback (bd-21d05e).
- Missing from PROJECT_SHOW_ARGS → bd-b76723 detector falsely
  warned 'These were ignored' for an honoured flag.
- --project + --name silent: --name won, no error message.
- positional + (--project|--name): hard error (correct).

## After state
- --name added to PROJECT_SHOW_ARGS (documented as bd-21d05e alias).
- New validator: --project + --name → 'cannot be combined' error
  matching positional+flag wording.
- Two pinning tests in caco-cli.
- Live verified: 'caco project show --name X' runs clean now.

## Diff summary
- `crates/caco-cli/src/lib.rs`: +67 / -1 — spec entry, conflict
  validator, two tests.
- cargo test-small green (2871 tests, +5 caco-cli); clippy clean.

## Operator-takeaway
A documented flag finally agrees with the dispatcher. Same-family
sweep beads (bd-a97ad4 bootstrap dev --check+--init-config,
bd-33b6d9 bd triage) remain — same pattern of 'help promises X,
dispatcher says Y'.
