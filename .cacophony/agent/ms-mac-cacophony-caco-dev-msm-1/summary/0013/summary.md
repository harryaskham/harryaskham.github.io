# Session 0013 — bd-c0c8b3: caco bd operator-actions CLI defects

## Outcome
P2 bug closed. Three operator-facing defects in `caco bd operator-actions`
fixed in one commit:
- `--include-closed` no longer silently HIDES open results
- `--limit 0` rejected eagerly with guidance
- `--limit bogus` rejected client-side instead of leaking daemon HTTP 400

## Context
After bd-fb4211 shipped I picked up bd-c0c8b3 from the open-bug ready
queue (P2, claimable, contained CLI fix). Test-user found three
related defects on cacophony 1.2.519 — all in the same dispatcher,
fixable together.

## Root causes

### Issue 1: --include-closed silently HID results
The flag previously dropped the daemon-side `status=open` filter,
relying on the daemon's default to return all statuses. On busy
projects the operator-action open beads were buried beyond limit=200
(recent in_progress + closed dominate), so toggling the flag *removed*
the open results — opposite of what "Also show recently-closed" implies.

### Issue 2 + 3: --limit edge cases
`--limit 0` parsed as literal 0, hit daemon, returned empty result.
`--limit bogus` propagated the daemon's raw HTTP 400.

## Fixes

1. **--include-closed**: when set, run two daemon queries (`status=open`
   and `status=closed`) sequentially and merge bead arrays before the
   client-side label filter. Both halves stay intact.
2. **--limit parser**: extracted `parse_operator_actions_limit()` as
   a free function. None/empty → 200 default; `0` → explicit error;
   non-numeric → explicit error with echoed value.

## Tests
- `parse_operator_actions_limit_handles_default_zero_and_garbage` —
  covers None/empty/0/positive/negative/non-numeric (1 test, 8 assertions)
- `cargo test-small`: 120 passed
- `cargo clippy -p caco-cli`: clean
- Manual verification on cacophony project: default returns 2 open
  beads; `--include-closed` now also returns 2 (no closed
  operator-action beads exist in recent 500 — separate concern
  bd-cdfeb5 about labels possibly stripped on close)

## Commit
`cf8edf79` — bd-c0c8b3: fix caco bd operator-actions --include-closed
+ --limit 0/garbage

## Decisions
- **Two queries, not one with no filter**: cheaper than fetching
  potentially thousands of mixed-status beads only to client-side
  filter; respects the operator's `--limit` per-half.
- **CLI-side --limit validation**: reject 0/garbage before any
  network round-trip. Cleaner UX than waiting for a daemon HTTP 400
  to surface.
- **Helper is free fn, not method**: easier to unit-test the parser
  rules without touching the dispatcher's reqwest plumbing.

## Open / next
- bd-cdfeb5 (labels stripped on close) is the deeper issue behind the
  empty closed-operator-action set — separate ticket, already filed.
- V2 workspace-view follow-ups (bd-689ee0/bd-b9e722/bd-ca37fc/bd-03f759/
  bd-18ef7e) explicitly not-yet-claimable per caco-ctrl; standing by.
