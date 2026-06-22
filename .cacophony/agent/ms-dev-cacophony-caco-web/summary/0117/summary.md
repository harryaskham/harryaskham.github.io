# Session summary — bd-c9a224: style.css .btn-primary family triple-+-paired dedup incl fully-dead-middle-block (bd-ed3d3f sibling)

## Goal
Continue bd-ed3d3f/bd-454506/.../bd-f479b0 dup-block audit. Family-triple-paired cycle, first 3-block cascade-chain dedup.

## Bead
- `bd-c9a224`

## The five blocks
1. `.btn-primary` x3:
   - 2952 canonical: 180deg nord9→nord10 + nord6 + nord10 + box-shadow.
   - 8223 middle: 135deg accent→accent-dim + accent + white. (Every declaration overridden by late.)
   - 8419 late: 135deg frost-3→frost-4 + frost-3 + nord6 RESTORED + position + overflow.
2. `.btn-primary:hover` x2:
   - 2959 canonical: 180deg nord8→nord9 + nord9 + box-shadow.
   - 8228 late: 135deg accent-hover→accent. (Override bg only.)

## Cascade-resolved truth
- .btn-primary: frost gradient + nord6 color + frost border + canonical box-shadow + position + overflow.
- .btn-primary:hover: accent-hover gradient + canonical border-color + canonical box-shadow.

## Fix
3 dead blocks deleted; canonical .btn-primary replaced with cascade-resolved truth; canonical :hover bg promoted; 3 marker comments. More-specific selectors `.btn-primary:hover:not(:disabled)` + `.btn-primary:focus-visible` preserved.

## Test (14 layers)
- 2x rule-head counts.
- 2x more-specific selector preservation.
- 2x cascade-resolved truth (incl box-shadow continuity from canonical).
- 3 NEGATIVE assertions on declarations-only (dead middle-block + 2x dead canonical).
- 2 marker pins + bd-ed3d3f sibling pin.

## NEW PATTERN (t): fully-dead-middle-block
In an N-block (N≥3) silent-override cascade chain, intermediate blocks where ALL declarations are overridden by later blocks contribute nothing to cascade-resolved truth. Detection: for each middle-block declaration, check whether a later block sets the same property (or a shorthand containing it). If every prop is shadowed, the middle block is fully dead and can be deleted with zero merge. Catalog entry #16.

The .btn-primary cycle is the FIRST 3-block cascade-chain dedup in this audit series; all prior chains were 2-block silent-overrides.

## Pattern combination
1x 3-block cascade-chain triple-merge (NEW pattern t) + 1x 2-block silent-override paired (bd-3dfff5/bd-238cea) + pattern (o) variant for higher-specificity sibling protection.

## Diff summary
- Files touched:
  - `crates/caco-web/static/style.css` -- 2 canonical merged; 3 dead blocks deleted (incl 1 fully-dead-middle-block); 3 marker comments.
  - `crates/caco-web/src/tests.rs` -- bd-c9a224 regression test (reuses strip_comments helper).
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*`.
- Net pass: 534 -> 535; 0 failures.

## Operator-takeaway
25 cycles, 68 wins. NEW pattern (t) fully-dead-middle-block introduced. Catalog now 17 patterns (a-q + s + t).
