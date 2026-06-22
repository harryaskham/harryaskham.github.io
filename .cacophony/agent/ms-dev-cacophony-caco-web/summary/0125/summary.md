# Session summary — bd-231a34: style.css 6x agent-type-chip/goal/action-body/bead/bar/batch hexa-paired dedup (bd-ef0924 sibling)

## Goal
Continue bd-ef0924/.../bd-f479b0 dup-block audit. Hexa-paired cycle (rolled back from octa-paired at edit time, pattern m catch #6).

## Bead
- `bd-231a34`

## The twelve blocks
1. `.agent-type-chip` strictly-additive (3 typography add + border-radius:4 byte-identical).
2. `.agent-goal-text` strictly-additive (border-left + padding-left disjoint).
3. `.action-card` body silent-override on `transition: all → transform 0.15s, box-shadow 0.15s`.
4. `.bead-description` pure-dead byte-identical-subset (max-height + overflow-y both byte-identical).
5. `.bar-segment` silent-override on transition (`flex var(--transition-slow), box-shadow var(--transition) → flex 0.3s ease`) with canonical survivors.
6. `.batch-action-bar` silent-override `backdrop-filter: blur(8) → blur(12)` + additive `-webkit-backdrop-filter: blur(12)` (vendor-prefix lockstep) + byte-identical border-radius.

## Fix
6 canonical merged + 6 dead blocks deleted + 6 marker comments.

## Test (24 layers)
- 6× rule-head counts (iterated for-loop).
- 6× cascade-resolved truth.
- 4 NEGATIVE on declarations-only.
- 1 pattern-m preservation pin (bd-b2b739 count==2).
- 6 marker pins + bd-ef0924 sibling pin.

## PATTERN (m) CATCH #6 — second rollback during implementation
`input[type="search"]::-webkit-search-cancel-button` + `:hover` were initially scoped for family-paired (s) silent-override merges. Pre-edit grep showed two heads each at adjacent line ranges — looked like clean dedup candidates. Initial implem ran the merge, which BROKE bd-b2b739 at test time (count==2 became count==1 for both heads).

**Root cause**: bd-b2b739 INTENTIONALLY preserved the two input-cancel blocks as separate "design eras" — early opacity-fade design at file top + late mask-recolouring design at file bottom — for visual documentation in the source even though cascade-resolved truth is one merged design (both contribute to render).

**Rollback procedure** (repeated from bd-1982d4 catch #5):
1. Detected via existing-test break (bd-b2b739 count==2).
2. Reverted the input cancel-button + :hover edits using single-target edits (3 reverts: body, hover, late stub).
3. Scope reduced from 8 paired-merges (octa) to 6 (hexa).
4. Added defensive pin in bd-231a34 test.

## NEW VERIFICATION LESSON (extends bd-1982d4 catch #5)
Pattern (m) check must include not just the SOURCE FILE verification (preceding line text), but ALSO a search of EXISTING TESTS for any rule-head count pin on the candidate selector. Fast check: `grep -nE "(count|2|3).*<selector>" crates/caco-web/src/tests.rs`. If a test pins the count > 1, the prior author had a reason (design preservation, cascade ordering, separation-of-concerns) and the merge must be SKIPPED.

**Pattern (m) catalog now: 6 catches** (bd-dfbeca, bd-46f5ea, bd-a6c685, bd-de61ec, bd-1982d4, bd-231a34). **Two of the six were rollbacks during implementation** — demonstrates the test-driven defense-in-depth value of the audit chain.

**Catalog refinement: pattern (m) sub-variant "intentional preserved duplicates"** — when a prior dedup pinned count > 1 because separation-of-concerns / design-era / cascade-ordering mattered, the rule is OFF-LIMITS for further merge regardless of grep results.

## Pattern combination
- 2× strictly-additive (.agent-type-chip, .agent-goal-text).
- 1× silent-override single (.action-card).
- 1× pure-dead byte-identical-subset (.bead-description).
- 1× silent-override mixed survival (.bar-segment).
- 1× silent-override + vendor-prefix lockstep + byte-id (.batch-action-bar).

## Diff summary
- Files touched:
  - `crates/caco-web/static/style.css` -- 6 canonical merged; 6 dead blocks deleted; 6 marker comments.
  - `crates/caco-web/src/tests.rs` -- bd-231a34 regression test (reuses block + strip_comments helpers).
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*`.
- Net pass: 542 -> 543; 0 failures.

## Operator-takeaway
33 cycles, 76 wins. Pattern (m) catch #6 with second rollback (defense via existing-test break). Catalog: 20 entries (a-q + s + t + u + v + w) + new sub-variant "intentional preserved duplicates" added to pattern (m) doc.
