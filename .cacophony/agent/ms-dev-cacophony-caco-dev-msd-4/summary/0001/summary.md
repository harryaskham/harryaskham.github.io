# Session summary — bd-2a4552 caco choices UX (alias + hint + default)

## Goal
Match bd-3a6078 pattern on choices: --id alias, not-found hint, sensible default --status.

## Bead(s)
- `bd-2a4552` — choices show --id rejected; not-found lacks hint; list default surfaces stale entries

## Before state
- `--id` rejected with bd-b76723 warning; only --choice-id worked.
- `choice bogus not found` with no discovery hint.
- `caco choices list` default --status=all returned 100 mostly-resolved choices.

## After state
- CHOICES_SHOW_ARGS registers --id alias; dispatch falls back from --choice-id to --id.
- Both text + JSON not-found responses include `(use 'caco choices list' to see available choices)`.
- `caco choices list` default --status=active; pass --status all to restore previous behaviour.
- Existing 4 tests updated to pin hint suffix.

## Diff summary
- `crates/caco-cli/src/lib.rs` (+47 / -17): arg spec, dispatch alias, render hint, list default, test updates.
- cargo test-small: 162 pass.

## Operator-takeaway
`caco choices show --id ch-123` now works. `caco choices list` returns the actionable subset by default. Pattern available for cross-cutting alias registry follow-up.
