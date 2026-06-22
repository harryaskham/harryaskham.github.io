# Session summary — bd-563b44: style.css 3x node-card-family/logo-mark triple-paired dedup (bd-a18fda sibling)

## Goal
Continue bd-a18fda/.../bd-f479b0 dup-block audit. Triple-paired cycle.

## Bead
- `bd-563b44`

## The six blocks
1. `.node-card` body silent-override on `transition: all → transform 0.15s, box-shadow 0.15s` (scope-limited 2-property transition for hover-lift performance).
2. `.node-card:hover, .node-card:focus-within` compound silent-override on transform `translateY(-3px) → -2px` and box-shadow (canonical `var(--shadow), 0 0 24px glow → 0 4px 14px rgba(0,0,0,0.3)`) with mixed-survival of canonical border-color.
3. `.logo-mark` silent-override on background gradient (3-stop `frost → blue → aurora-purple` → 2-stop accent simplification).

## Fix
3 canonical merged + 3 dead blocks deleted + 3 marker comments.

## Test (15 layers)
- 3x rule-head counts (incl `.node-card:focus-within` compound-head pin).
- 3x cascade-resolved truth.
- 3 NEGATIVE on declarations-only (1 per merge, incl 3-stop gradient defense).
- 3 marker pins + bd-a18fda sibling pin.

## NEW PATTERN (v): cascade-dead-elaborate-canonical
The `.logo-mark` 3-stop frost→aurora-purple gradient was in the canonical text but NEVER rendered because the late block silently overrode it. Merging eliminates the misleading dead 3-stop gradient string from style.css. Marker explicitly documents this so future readers don't think they're seeing an out-of-date late override and "restore" the dead 3-stop gradient.

This is the FIRST time the audit chain has eliminated a visually-rich dead canonical declaration. Sub-variant of pattern (m) byte-identical-subset, but **inverted**: instead of late being subset of canonical, the canonical's RICHER form is subset (in visible effect) of the late's SIMPLER form.

**Catalog #19**: pattern (v) cascade-dead-elaborate-canonical. Likely applicable to other "dead-rich" declarations elsewhere (e.g., complex shadow/gradient/transform fallbacks that later got simplified).

## Pattern combination
- Family-paired (s) from bd-ed3d3f applied to .node-card body + compound.
- Silent-override with mixed-survival from bd-c9a224 lineage on the compound.
- Pattern (v) NEW on .logo-mark.

## Diff summary
- Files touched:
  - `crates/caco-web/static/style.css` -- 3 canonical merged; 3 dead blocks deleted; 3 marker comments.
  - `crates/caco-web/src/tests.rs` -- bd-563b44 regression test (reuses strip_comments helper).
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*`.
- Net pass: 540 -> 541; 0 failures.

## Operator-takeaway
31 cycles, 74 wins. NEW pattern (v) cascade-dead-elaborate-canonical. Catalog: 19 patterns (a-q + s + t + u + v).
