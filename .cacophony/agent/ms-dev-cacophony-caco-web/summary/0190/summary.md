# Session summary — bd-c9e68a: Pattern (z) sweep, 2 more orphan bugs

## Goal
Pattern (z+1) NEW sweep-after-discovery; fix 2 more orphan-selector-list bugs.

## Bead
- `bd-c9e68a`

## Discovery
- Python regex sweep after bd-458634; 5 candidates; 2 REAL.

## Site A: .inbox-card-time orphan
- Was getting body-paragraph styling instead of small/faint/mono timestamp.

## Site B: .action-card / .project-card orphan
- Headless chromium verified: opacity=0, position=absolute, pointerEvents=none in production.

## Fix
- Site A: restored intended rule body matching .inbox-card-meta canonical.
- Site B: removed orphan lines so rule matches only ::before as intended.
- Both preserve prior marker comments verbatim (Pattern x echo).

## Regression test (~90 lines)
- Inbox-card-time rule shape + canonical body.
- No orphan precedes .action-card::before.

## Pattern (z+1) NEW: SWEEP-AFTER-DISCOVERY DISCIPLINE
- After adding a new Pattern entry, sweep for siblings before next routine work.
- Same merge-author often wrote multiple sites the same way.

## Operator-visible effect
- SITE A: inbox timestamps now small/faint/mono.
- SITE B: action/project cards no longer get position:absolute/opacity:0/pointer-events:none from absorbing.

## Diff summary
- `crates/caco-web/static/style.css` -- Site A recovered rule + Site B orphan lines removed; bd-de61ec / bd-ed3d3f marker comments preserved verbatim.
- `crates/caco-web/src/tests.rs` -- bd-c9e68a forward-guard (~90 lines).
- Net pass: 604 -> 605; 0 failures.

## Operator-takeaway
98 cycles, 139 wins. **2 MORE SHIPPED BUGS FIXED**. Pattern (z+1) NEW sweep-after-discovery discipline validated immediately by 2 sibling-bug surfacings. Pattern catalog: 33 entries.
