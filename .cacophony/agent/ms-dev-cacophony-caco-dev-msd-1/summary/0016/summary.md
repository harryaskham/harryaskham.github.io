# Session summary — bd-4e8d50 log perf-list validators

## Goal
Bring caco log perf-list up to the gold-standard validator
surface of caco log exceptions (its sister subcommand): proper
--limit + --since validation, no raw HTTP 400 leak, and rich
header summary.

## Bead(s)
- `bd-4e8d50` (P3 bug, test-user) — log perf-list broken-sibling.

## Before state
- --limit 0 → silently used default 50.
- --limit bogus → raw HTTP 400 from daemon parse.
- --since absent → warned-and-ignored as unknown flag.
- Output header: terse '5 event(s), 67092 total'.

## After state
- --limit validated via validate_positive_limit (gold-standard wording).
- --since DURATION|RFC3339 parsed at CLI boundary (mirrors exceptions).
- Over-fetch + post-filter when --since set (10x strategy).
- Header: 'perf events (N total, M after --since filter, showing K)'.
- Three pinning tests in caco-cli.

## Diff summary
- `crates/caco-cli/src/lib.rs`: +163 / -7 — spec, dispatch wiring,
  validators, header rewrite, tests.
- cargo test-small green (2871 tests, +7); clippy clean.

## Operator-takeaway
caco log perf-list now behaves like caco log exceptions for typo
inputs and supports --since 30m for time-bounded queries. Bug
family (bd-7b8641, bd-f0fc5b) remains for the cross-cutting
'integer-flag-leaks-HTTP-400' fix on other surfaces.
