# Session summary — bd-cdfeb5 operator-actions list surface unification

## Goal

Fix the bug where `caco operator-actions list` silently missed beads
that used `label=operator-action` (vs `[operator-action]` title prefix),
causing the operator to see incomplete results compared to
`caco bd operator-actions`.

## Bead(s)

- `bd-cdfeb5` — caco operator-actions list and caco bd operator-actions
  disagree on the operator-action set

## Before state

- Failing tests: none relevant
- Two duplicate match arms for `operator-actions list` in caco-cli
  dispatch: first arm (bd-6b7b30) called legacy
  `dispatch_operator_actions_list()` that only matched title prefix;
  second arm (bd-340a4f) routed to canonical
  `dispatch_bd_operator_actions()` but was dead code (Rust takes first
  match).
- Result: beads with label=operator-action but no title prefix (e.g.
  bd-c24ff7) were invisible to the top-level surface.

## After state

- Failing tests: none; cargo test-small all pass, clippy clean
- Single match arm through `dispatch_bd_operator_actions()`
- Legacy `dispatch_operator_actions_list()` (99 lines) deleted entirely
- Both surfaces now produce identical results with full flag support

## Diff summary

- Commits: 925cefcb130e
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: 0 new (existing `is_operator_action_bead_detects_label_and_title_prefix` already covers)
- Behavioural delta: `caco operator-actions list` now includes label-only
  beads and accepts --project/--max-age/--limit/--include-closed flags.
  Net -116 / +14 lines (dead code removal).

## Operator-takeaway

Duplicate match arms in Rust's match expression silently shadow later
arms. This caused the label-based operator-action beads to be invisible
to the top-level command while `caco bd operator-actions` showed them
correctly. The fix was a simple dead-code removal, but the operational
impact was real: the operator could miss action items. Worth a lint or
test asserting the two surfaces agree for future regressions.
