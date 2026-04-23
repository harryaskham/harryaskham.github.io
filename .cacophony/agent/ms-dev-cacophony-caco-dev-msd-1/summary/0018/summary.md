# Session summary — bd-a97ad4 bootstrap dev mutual exclusion

## Goal
Enforce the "One mode per invocation" rule that bootstrap dev's
help text already promises, so a combined invocation like
'--check --init-config' errors loudly instead of silently dropping
the safer half.

## Bead(s)
- `bd-a97ad4` (P3 bug, test-user) — bootstrap dev mode conflict.

## Before state
- Help: 'One mode per invocation: --check / --init-config / ...'
- Dispatcher: silently picks first matching branch (--check first).
- `bootstrap dev --check --init-config` → silently runs init-config,
  drops --check. Operator gets no warning that the diagnostic step
  they typed never ran.

## After state
- Validator at start of bootstrap dev arm collects every supplied
  mode flag; >1 → 'only one mode allowed per invocation. Got: X, Y.
  Pick one (bd-a97ad4)' error listing every offender.
- Single-mode and zero-flag (unified) invocations unchanged.
- Two pinning tests in caco-cli.

## Diff summary
- `crates/caco-cli/src/lib.rs`: +108 — validator + 2 tests.
- cargo test-small green (2878 tests, +5 caco-cli); clippy clean.

## Operator-takeaway
Three sibling beads in one family now closed:
  - bd-2c88ed project show --project + --name
  - bd-a97ad4 bootstrap dev --check + --init-config (this bead)
  - bd-33b6d9 bd triage (already closed elsewhere)
A future cleanup that adds a new mode flag must also extend the
validator's `chosen` collector — caught by the help-shape test.
