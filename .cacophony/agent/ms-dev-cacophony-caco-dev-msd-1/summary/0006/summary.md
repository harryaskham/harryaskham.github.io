# Session summary — bd-62f10c sparse-cone .cacophony baseline

## Goal

Fix the operator footgun where a sparse include list that omits
`.cacophony` leaves the cacophony control plane sparse-excluded and
defeats drift detection forever (catch-22 with checkout-state.json).

## Bead(s)

- `bd-62f10c` (P1 bug) — Sparse cone must always include `.cacophony/`
  implicitly; operator must not be able to opt out.

## Before state

- `apply_sparse_checkout` passed the operator's `include` list
  verbatim to `git sparse-checkout set`.
- `path_included` consulted only the operator's spec.
- Symptom: picasso-health checkout had `.cacophony/` empty,
  `checkout-state.json` un-persistable, drift detection broken
  even after operator added `.cacophony` to projects.yaml.
- 0 tests covering the baseline-injection invariant.

## After state

- New `REQUIRED_SPARSE_BASELINE = [".cacophony"]` constant in
  `crates/caco-daemon/src/checkout.rs`.
- `apply_sparse_checkout` injects baseline first in both cone and
  no-cone modes, deduplicating against operator-supplied entries.
- No-cone mode: any operator exclude that targets a baseline path
  is silently dropped.
- `path_included` reports baseline paths as unconditionally
  included so `caco config sparse validate` matches actual
  materialisation.
- Documented invariant on `SparseConfig` in
  `crates/caco-config/src/model.rs`.
- 3 new tests pin the invariant; 9/9 sparse tests green.

## Diff summary

- Files: `crates/caco-daemon/src/checkout.rs`,
  `crates/caco-config/src/model.rs`.
- Behavioural delta: production behaviour now guarantees
  `.cacophony/` materialisation regardless of operator config.
- Tests: +3 (cone-without-cacophony, nocone-cannot-exclude,
  path_included-unconditional).
- Cargo test-small: 2837+ workspace tests green; clippy clean.

## Out of scope (deferred)

- Secondary defence-in-depth: move `checkout-state.json` into
  `.git/cacophony-checkout-state.json` so it survives any future
  bug that re-excludes `.cacophony`. Bead description mentions
  this as a "defence in depth" follow-up; primary fix is sufficient
  to close the immediate symptom.
- `caco config validate --strict` warning when operator include
  list redundantly mentions `.cacophony` (cosmetic).

## Operator-takeaway

Operators no longer need to remember to include `.cacophony` in
their projects.yaml sparse spec. The daemon now treats this as a
hardcoded baseline that cannot be opted out of, in both cone and
no-cone modes. Existing operator configs that mention `.cacophony`
explicitly are unaffected (deduplicated). The original
picasso-health symptom is fixed at the root cause.
