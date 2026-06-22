# Session summary — bd-1cebc1: wontfix audit-decision documentation

## Goal
Document a wontfix proposal so future audits don't re-investigate.

## Bead
- `bd-1cebc1` (wontfix)

## Audit
- Proposed: flip index.html title to page-first form for parity.
- Found prior bd-fa3d05 forward-guard intentionally preserves brand-first form because the app.js dynamic title generator uses `{badge}{view} · Cacophony Dashboard`.

## Fix
- tests.rs comment block: + wontfix rationale referencing bd-1cebc1 with dynamic-title coupling explanation.

## Why
- Saves next audit time re-investigating the dynamic-title coupling.

## Diff summary
- `crates/caco-web/src/tests.rs` -- comment-only documentation.
- Net pass: 586 -> 586; 0 failures.

## Operator-takeaway
78 cycles. wontfix slice documented with rationale; saved for future audits. Pattern catalog: 22 entries.
