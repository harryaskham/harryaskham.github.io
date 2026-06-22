# Session summary — bd-2209dd: dead ::selection block dedup

## Goal
Pattern (m) family-paired dedup: remove silently-overridden ::selection block.

## Bead
- `bd-2209dd`

## Audit
- L180: rgba transparent (DEAD, overridden).
- L7737: var(--nord10) solid (TRUTH).
- L9447: input-scoped (UNRELATED).

## Fix
- Remove dead L180 block + rationale comment.

## Why
- Cascade truth preservation; dead-code removal; Pattern (m) extension.

## Regression test (~55 lines)
- Line-prefix filter for global ::selection (excludes input-scoped).
- Cascade-truth literal preservation.
- Firefox-prefix sibling parity.

## Operator-visible effect
- None. Cascade truth already was the visible behavior.

## Diff summary
- `crates/caco-web/static/style.css` -- dead block removed + rationale comment.
- `crates/caco-web/src/tests.rs` -- new bd-2209dd forward-guard (~55 lines).
- Net pass: 597 -> 598; 0 failures.

## Operator-takeaway
90 cycles, 132 wins. Pattern (m) family-paired dedup (extending bd-3dfff5/bd-9b7346/bd-c41f42 family). Pattern catalog: 29 entries.
