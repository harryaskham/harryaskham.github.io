# Session summary — bd-ef0924: style.css 6x agent-meta-chip(s)/agent-summary-card(:hover)/agent-nudge-input(:focus) hexa-paired dedup (bd-563b44 sibling)

## Goal
Continue bd-563b44/.../bd-f479b0 dup-block audit. **HEXA-paired cycle (densest to date; previous max 5)**.

## Bead
- `bd-ef0924`

## The twelve blocks (3 family-paired groups)

### Family A: `.agent-meta-chips` + `.agent-meta-chip` (pattern s)
1. `.agent-meta-chips` strictly-additive (margin-top:6px) + 3 byte-identical layout decls.
2. `.agent-meta-chip` silent-override mixed survival (padding `3px 9px → 2px 8px`, bg `bg-glass → bg-active`); 6 canonical-only survivors.

### Family B: `.agent-summary-card` + `:hover` (pattern s)
3. `.agent-summary-card` body silent-override on `transition: all → transform 0.15s, box-shadow 0.15s`.
4. `.agent-summary-card:hover` silent-override transform DIRECTION CHANGE (`translateX(3px) → translateY(-1px)`) + additive box-shadow + canonical bg/border-color survivors. **PATTERN (v) catch #2** (dead-direction).

### Family C: `.agent-nudge-input` + `:focus` (pattern s)
5. `.agent-nudge-input` body silent-override font-size `12.5 → 12` + additive font-family `var(--font-mono)`.
6. `.agent-nudge-input:focus` silent-override box-shadow `0 0 0 3px rgba(.15) → 0 0 0 2px var(--accent-soft)`; canonical outline + border-color survivors.

## Fix
6 canonical merged + 6 dead blocks deleted + 6 marker comments (4 individual + 2 family-paired group markers).

## Test (24 layers)
- 6x rule-head counts (iterated for-loop array).
- 5x cascade-resolved truth.
- 5 NEGATIVE on declarations-only (incl pattern-v dead-direction defense).
- 3 marker pins + bd-563b44 sibling pin.

## NEW INFRASTRUCTURE: helper `block()` closure
`block()` closure helper added to deduplicate block-extraction across 6 selectors. Uses `format!()` to construct unique find-needles. Hexa-paired cycles benefit; without it the boilerplate would have been ~30 lines, helper makes it 4-line per-selector.

**Catalog #20**: helper (w) block-extractor function closure (sibling of helper (q) strip_comments).

## Pattern-v catch #2
`.agent-summary-card:hover` transform direction change (translateX horizontal slide → translateY vertical lift, matching the rest of the dashboard's lift-on-hover convention). The dead translateX form never rendered.

## CSS cascade note (in marker)
`.agent-nudge-input` merge: `font-family: var(--font-mono);` declared AFTER `font: inherit;` wins for the font-family component (long-form wins over shorthand component when declared later in the same rule). Marker explicitly documents this so future readers don't strip font-family as redundant.

## Pattern combination
3 family-paired (s) + 4 silent-override + 2 strictly-additive + 1 mixed-survival + 1 pattern (v) catch + 1 cascade-component note + helper (w) introduction.

## Diff summary
- Files touched:
  - `crates/caco-web/static/style.css` -- 6 canonical merged; 6 dead blocks deleted; 6 marker comments.
  - `crates/caco-web/src/tests.rs` -- bd-ef0924 regression test (introduces `block()` helper).
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*`.
- Net pass: 541 -> 542; 0 failures.

## Operator-takeaway
32 cycles, 75 wins. Densest cycle to date (hexa-paired). Catalog: 20 entries (a-q + s + t + u + v + w).
