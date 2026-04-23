# Session summary — bd-4a2bd9 caco cron run safety flags

## Goal

Stop bare `caco cron run --name X` from fanning out to every
configured node when an operator pre-flights with `--dry-run` or
restricts to `--node NAME`. The unknown-flag warning fires
post-dispatch, defeating the purpose.

## Bead(s)

- `bd-4a2bd9` (P2 bug, test-user) — Triggered actual speaking-clock
  fan-outs while pre-flighting; pattern same as bd-b327cd /
  bd-06d736 (destructive command with no guardrails).

## Before state

- `caco cron run --name X --dry-run` → unknown-flag warning,
  cron fired on every target node anyway.
- `caco cron run --name X --node bogus` → unknown-flag warning,
  cron fired on every target node anyway.
- 0 tests pinning the cron-run flag surface.

## After state

- `--dry-run` resolves target nodes + command, prints/JSON-emits
  the plan, exits without dispatching. Includes a re-run hint
  naming the count.
- `--node NAME` restricts the dispatch to one configured node;
  errors loudly with the cron's full target set if NAME is not a
  member (so a typo cannot silently fall back to fan-out).
- Existing `--local` short-circuit preserved.
- 1 new test pins `--dry-run`, `--node`, `--local` in
  CRON_RUN_ARGS.

## Diff summary

- `crates/caco-cli/src/lib.rs`: +105 / -2 — two new ArgSpecs,
  dispatch entry point updates, dry-run + node-filter logic in
  dispatch_cron_run, one new test.
- Behavioural delta: `--dry-run` and `--node` now do what their
  names imply instead of being silently dropped.
- cargo test-small workspace-wide green; clippy clean for caco-cli.

## Operator-takeaway

Together with bd-06d736 (test run safety gate landed this session),
this hardens the two highest-fan-out destructive surfaces flagged
by the test-user agent. The `--dry-run` plan output is intentionally
verbose (one line per target node + a re-run hint) so the operator
sees exactly what *would* happen before committing.
