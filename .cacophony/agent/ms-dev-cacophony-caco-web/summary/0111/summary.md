# Session summary — bd-2e59b2: style.css 4x project-grid selectors quad-paired dedup (bd-3cb20a sibling)

## Goal
Continue bd-3cb20a/.../bd-f479b0 dup-block audit. Quad-paired cycle in projects-grid family.

## Bead
- `bd-2e59b2`

## The eight blocks
1. `.project-stat` (silent-override): padding+border-radius+transition; text-align:center survives.
2. `.project-stat:hover` (byte-identical dup).
3. `.project-card` (silent-override on transition: border-color/transform var -> transform/box-shadow 0.15s). PRESERVED `.project-card:hover, .project-card:focus-within` compound rule.
4. `.project-bead-bar` (byte-identical SUBSET dup).

## Fix
4 canonical merged + 4 late deleted + 4 marker comments + 1 compound-rule explicitly re-emitted in deletion edit.

## Test (14 layers)
- 4x rule-head counts.
- 4x baseline+promoted/preserved declaration sets.
- 2 NEGATIVE assertions (silent-override dead).
- 1 explicit compound-rule preservation assertion.
- 4 marker pins + bd-3cb20a sibling pin.

## NEW PATTERN (o): explicit-compound-sibling-protection
When a late block to delete is immediately followed by a compound-rule sibling (e.g. `.project-card:hover, .project-card:focus-within { ... }`) that should remain active, the deletion edit must INCLUDE the compound rule in oldText and RE-EMIT it in newText to prove preservation. Test must explicitly assert the compound rule survives. Pattern (o) added to the family-chain catalog.

## Pattern combination
2x silent-override (bd-3dfff5/bd-238cea) + 2x byte-identical SUBSET dup (bd-c4e2ee/bd-f0d094) + compound-rule-adjacent preservation = 3 distinct patterns in one quad-paired cycle. Largest cycle yet.

## Diff summary
- Files touched:
  - `crates/caco-web/static/style.css` -- 4 canonical merged; 4 dead blocks deleted; 4 marker comments.
  - `crates/caco-web/src/tests.rs` -- bd-2e59b2 regression test (14 layers including compound-rule preservation).
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*`.
- Net pass: 528 -> 529; 0 failures.

## Operator-takeaway
19 cycles, 62 wins. NEW pattern (o) introduced: explicit-compound-sibling-protection. style.css continues shrinking.
