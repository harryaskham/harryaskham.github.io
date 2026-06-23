# Session summary — bd-5f84af (document test-small gate coverage + echo-gate source-pin hazard)

## Goal

Stop the recurring controller churn over "does this broken-on-main gate reints?"
by documenting the reint-gate's test-small crate-composition and the two framings
tonight's churn revealed — using the deep context I built while fixing my own
bd-7e5dfe -> bd-eed5e0 source-pin regression.

## Bead(s)

- `bd-5f84af` — Document reint-gate test-small crate-composition (exclusions) to
  prevent broken-main gating confusion.

## Before state

- AGENTS.md already documented the `test-small` exclusion list
  (caco-cli/caco-daemon/caco/caco-sidecar) + that `cargo check --workspace
  --tests` only compiles -> runtime failures in those 4 are non-gating
  (bd-38bab0). But it did NOT capture: (a) the echo-disabled-gate framing
  ("blocks clean self-validation" vs "the gate blocks reints"), or (b) the
  source-pin landing hazard for IN-test-small UI crates.

## After state

- AGENTS.md gate-coverage paragraph now also states: while the gate is
  echo-disabled (bd-ff92cd), a broken-on-main blocks clean test-small
  self-validation, not reint-time landing; verify crate-vs-test-small membership
  (caco-tui/caco-web ARE in test-small) before asserting what gates a reint; and
  source-pin/`*_contract` tests in IN-test-small crates land red silently on a
  pinned-source change (only compiled by cargo check) -> run the crate's FULL lib
  tests + git grep the needle before landing (bd-eed5e0 cited as the concrete
  case).

## Diff summary

- Code/content commit: 749beb939 (final landed squash SHA from the receipt).
- Files touched: AGENTS.md (1 paragraph, additive prose).
- Tests: n/a (docs-only; reint gate auto-skips, bd-45114c).
- Behavioural delta: none (documentation).

## Operator-takeaway

This closes the loop on tonight's 3 gate-blocks: the durable fix is re-enabling
the real gate (bd-ff92cd), but until then the operative reality is "self-validate,
because the echo gate won't" — and source-pin UI-crate tests are the specific
silent-landing trap (my bd-eed5e0 regression). The doc now tells the next agent to
run full crate tests + grep the pin needle when touching pinned source.
