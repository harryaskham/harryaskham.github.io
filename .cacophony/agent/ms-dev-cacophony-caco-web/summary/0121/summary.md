# Session summary — bd-1982d4: style.css 3x filter-chip/data-table/chat-message triple-paired dedup (bd-45aa52 sibling)

## Goal
Continue bd-45aa52/.../bd-f479b0 dup-block audit. Triple-paired cycle (originally planned as quad-paired; rolled back from pattern-m catch #5).

## Bead
- `bd-1982d4`

## The six blocks
1. `.filter-chip.active` byte-identical-subset pure-dead (late only redeclares 3 props all byte-identical with canonical).
2. `.data-table tbody tr:hover` strictly-additive (canonical box-shadow + late background:rgba !important; !important qualifier preserved).
3. `.chat-message:hover` silent-override on background with mixed survival (border-left + transform from canonical preserved; bg overridden lighter accent).

## Fix
3 canonical merged + 3 dead blocks deleted + 3 marker comments.

## Test (12 layers)
- 3x rule-head counts.
- 3x cascade-resolved truth.
- 1 NEGATIVE on dead bg (strip_comments helper).
- 1 pattern-(m) preservation pin (bd-c4e2ee count==2).
- 3 marker pins + bd-45aa52 sibling pin.

## PATTERN (m) CATCH #5 — at edit time (rollback)
.nav-item.active .nav-icon was originally scoped for adjacent additive merge. Pre-edit grep showed two heads at adjacent lines 456 and 460. Initial implementation ran the merge, which BROKE bd-c4e2ee at test time. Root cause: the grep at L456 matched the LAST selector line of a 3-line compound rule `.nav-item:hover .nav-icon, .nav-item:hover .nav-icon, .nav-item.active .nav-icon { opacity: 1; }`. Merging would have erroneously applied the active-only drop-shadow to :hover states too.

**Rollback procedure**:
1. Detected via existing-test break (bd-c4e2ee count==2).
2. Reverted the merge with single-target edit.
3. Scope reduced from 4 paired-merges to 3.
4. Added defensive pin in bd-1982d4 test.

**New verification lesson**: pattern (m) verification must include READING THE ACTUAL LINE TEXT around each grep hit. Fast check: `sed -n 'L-3,L+1p' file | grep -c ','` — if any comma in the previous 3 lines, the hit is LIKELY a compound-rule tail.

Pattern (m) catalog now: 5 catches (bd-dfbeca, bd-46f5ea, bd-a6c685, bd-de61ec, bd-1982d4).

## Pattern combination
1x byte-identical-subset pure-dead (bd-c4e2ee lineage) + 1x strictly-additive (bd-f0393a) + 1x silent-override mixed survival (bd-c9a224).

## Diff summary
- Files touched:
  - `crates/caco-web/static/style.css` -- 3 canonical merged; 3 dead blocks deleted; 3 marker comments.
  - `crates/caco-web/src/tests.rs` -- bd-1982d4 regression test (reuses strip_comments helper).
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*`.
- Net pass: 538 -> 539; 0 failures.

## Operator-takeaway
29 cycles, 72 wins. Pattern (m) catch #5 with first rollback (defense-in-depth via existing-test break detection). Catalog now has documented rollback procedure.
