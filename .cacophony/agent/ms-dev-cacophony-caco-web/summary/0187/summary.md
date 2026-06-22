# Session summary — bd-93be3d: workspace accent outlier promotion

## Goal
Pattern (y) NEW: canonical-grouping discipline.

## Bead
- `bd-93be3d`

## Audit
- 6 raw hex hits outside :root; 1 actionable outlier.
- Established pattern: 13 sibling rules in canonical group.

## Fix
- Promote outlier into canonical group at L8740.
- Token form + explicit soft variant + selector prefix.
- Rationale comment at former L10396.

## Why
- Token consistency, specificity consistency, explicit > implicit, single source of truth.

## Regression test (~55 lines)
- Count==14 + workspace rule shape + no raw hex invariant.

## Pattern (y) NEW: CANONICAL-GROUPING DISCIPLINE
- Outliers using different form/specificity/completeness should be promoted.
- Enforcement: count-pinned + outlier-form negative assertion.

## Operator-visible effect
- None. Same accent color; explicit soft fallback now.

## Diff summary
- `crates/caco-web/static/style.css` -- L8740 inserted + L10396 outlier replaced with rationale comment.
- `crates/caco-web/src/tests.rs` -- new bd-93be3d forward-guard (~55 lines).
- Net pass: 601 -> 602; 0 failures.

## Operator-takeaway
95 cycles, 136 wins. Pattern (y) NEW canonical-grouping discipline. Pattern catalog: 31 entries.
