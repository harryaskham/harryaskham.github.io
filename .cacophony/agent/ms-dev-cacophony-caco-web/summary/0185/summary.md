# Session summary — bd-03f8e9: .chat-body a shorthand-reset dedup

## Goal
Pattern (m) family-paired dedup + shorthand-reset trap awareness.

## Bead
- `bd-03f8e9`

## Audit
- L2435/L2440: color:nord8 + text-decoration:underline + text-decoration-color:rgba (all DEAD).
- L7856/L7861: color:accent + text-decoration:underline dotted/solid (TRUTH).
- Shorthand-reset: text-decoration shorthand wipes text-decoration-color longhand.

## Fix
- Promote cascade-winning values into L2435/L2440.
- Remove L7856-7864 dup blocks.

## Why
- Cascade truth + shorthand-reset awareness + single source of truth.

## Regression test (~62 lines)
- Exactly-1 base + hover blocks, cascade-truth + dead-value negative invariants.

## Pattern (m) extension — SHORTHAND-RESET TRAP
- text-decoration shorthand RESETS all 3 longhand sub-properties.
- When late block sets a shorthand, ALL longhand sub-properties in earlier blocks are dead.
- Added to Pattern (m) audit checklist.

## Operator-visible effect
- None. Cascade truth was already visible.

## Diff summary
- `crates/caco-web/static/style.css` -- L2435+L2440 promoted + L7856-7864 removed + 3 rationale comments.
- `crates/caco-web/src/tests.rs` -- new bd-03f8e9 forward-guard (~62 lines).
- Net pass: 600 -> 601; 0 failures.

## Operator-takeaway
93 cycles, 135 wins. Pattern (m) family-paired dedup + SHORTHAND-RESET TRAP awareness. Pattern catalog: 29 entries (Pattern (m) strengthened).
