# Session summary — bd-45aa52: style.css 4x btn-xs/btn-ghost/chat-body-code/content quad-paired dedup (bd-ae48ac sibling)

## Goal
Continue bd-ae48ac/.../bd-f479b0 dup-block audit. Quad-paired cycle.

## Bead
- `bd-45aa52`

## The eight blocks
1. `.btn-xs` silent-override on padding (2px 4px → 2px 6px) + additive font-size:10px.
2. `.btn-ghost:hover` silent-override on border-color (transparent → var(--border-strong)).
3. `.chat-body code` silent-override on 3 props (bg, padding, font-size 12px → 0.9em relative) with mixed survival (canonical-only border + color preserved; font-family + border-radius byte-identical).
4. `#content` strictly-additive 2 custom properties (--view-accent, --view-accent-soft).

## Fix
4 canonical merged + 4 dead blocks deleted + 4 marker comments.

## Test (17 layers)
- 4x rule-head counts.
- 4x cascade-resolved truth.
- 3 NEGATIVE on declarations-only (strip_comments helper).
- 4 marker pins + bd-ae48ac sibling pin.

## Notable cleanup
- `.chat-body code` font-size went from `12px` absolute to `0.9em` relative; with the merge documented, future readers understand why code spans inside chat scale with parent font-size instead of being a fixed 12px island.
- `#content` additive merge preserves inheritance scope for `.view.active .view-header h2::after { color: var(--view-accent); }`.

## Pattern combination
2x silent-override (bd-3dfff5/bd-238cea lineage) + 1x silent-override with mixed survival (bd-c9a224 lineage) + 1x strictly-additive custom-property merge (bd-f0393a lineage on a new property class).

## Diff summary
- Files touched:
  - `crates/caco-web/static/style.css` -- 4 canonical merged; 4 dead blocks deleted; 4 marker comments.
  - `crates/caco-web/src/tests.rs` -- bd-45aa52 regression test (reuses strip_comments helper).
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*`.
- Net pass: 537 -> 538; 0 failures.

## Operator-takeaway
28 cycles, 71 wins.
