# Session summary — bd-360942: style.css 6x kbd-hint/badge/bead-detail-meta(dt+dd)/agent-detail-hero/chat-slash-suggest hexa-paired dedup (bd-231a34 sibling)

## Goal
Continue bd-231a34/.../bd-f479b0 dup-block audit. Hexa-paired cycle.

## Bead
- `bd-360942`

## The twelve blocks (4 individual + 1 family-paired)
1. `.kbd-hint` silent-override mixed survival (color `text-dim → text-muted, #888`, font-size `10px → 0.8rem`).
2. `.badge` strictly-additive (`font-variant-numeric: tabular-nums` for tabular badge counts).
3-4. `.bead-detail-meta dt+dd` family-paired (s):
    - dt silent-override font-size `11→12` + byte-id color.
    - dd pure-dead byte-identical-subset.
5. `.agent-detail-hero` silent-override mixed (margin-bottom `12→16`) + 3 byte-id.
6. `.chat-slash-suggest` 3 silent-overrides (border, box-shadow, backdrop-filter blur 8→12) + 1 additive vendor-prefix lockstep (-webkit-backdrop-filter) + 1 byte-id.

## Fix
6 canonical merged + 6 dead blocks deleted + 6 marker comments.

## Test (24 layers)
- 6× rule-head counts (iterated for-loop).
- 6× cascade-resolved truth.
- 5 NEGATIVE on declarations-only.
- 6 marker pins + bd-231a34 sibling pin.

## Pattern (m) preflight applied per bd-231a34 catch #6 lesson
- Source-file check: 0 commas in 3 prev lines for selected 6.
- Test-pin grep: no count pins on selected 6.

**3 candidates SKIPPED**:
- `.freshness-indicator` (compound 4-selector tail at L8644; pattern m source-file catch).
- `.chat-body a` (text-decoration shorthand cascade — late's `text-decoration: underline solid` would reset canonical's `text-decoration-color:rgba(136,192,208,0.4)`; risk regression).
- `.chat-avatar` (display `inline-flex → flex` direction change in late; could affect inline-flow contexts; risk regression).

## Pattern (v) note on .kbd-hint
Color change from `var(--text-dim)` to `var(--text-muted, #888)` is a cascade-dead-elaborate-canonical sub-variant — both vars are similarly dim but late explicitly provides a hex fallback for var resolution failures. Marker documents this so future readers don't restore text-dim under the assumption that the explicit hex fallback was unintended.

## Pattern combination tally
- 2× silent-override mixed survival (.kbd-hint, .agent-detail-hero).
- 1× strictly-additive (.badge).
- 1× silent-override single + 1 byte-id (.bead-detail-meta dt).
- 1× pure-dead byte-identical-subset (.bead-detail-meta dd).
- 1× silent-override + vendor-prefix lockstep + byte-id (.chat-slash-suggest).

Family-paired (s) coverage: .bead-detail-meta dt+dd.

## Diff summary
- Files touched:
  - `crates/caco-web/static/style.css` -- 6 canonical merged; 6 dead blocks deleted; 6 marker comments.
  - `crates/caco-web/src/tests.rs` -- bd-360942 regression test.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*`.
- Net pass: 543 -> 544; 0 failures.

## Operator-takeaway
34 cycles, 77 wins. Hexa-paired with 3 SKIPPED candidates documented (compound-tail, shorthand-cascade, display-direction). Catalog: 20 entries (a-q + s + t + u + v + w).
