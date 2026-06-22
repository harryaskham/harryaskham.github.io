# Session summary — bd-debc7c: style.css 3x ws-stat-card/ws-chat-msg/nav-key tri-paired dedup (bd-360942 sibling)

## Goal
Continue bd-360942/.../bd-f479b0 dup-block audit. Tri-paired cycle (smaller than recent hexa, after pattern (m) preflight skipped 4 of 7 candidates as compound-tails / intentional split).

## Bead
- `bd-debc7c`

## The six blocks
1. `.ws-stat-card` strictly-additive (`position: relative; overflow: hidden;` for ::before/::after layering).
2. `.ws-chat-msg` strictly-additive (`animation: wsMsgIn 0.18s ease-out;` entry animation; @keyframes definition preserved in place).
3. `.nav-key` silent-override mixed survival (5/9 promoted: font-size 9.5→10, bg `rgba(0,0,0,0.2)→var(--bg-primary)`, padding `2px 5px→1px 4px`, border-radius `4→3`, opacity `0→0.5`); 2 byte-id; canonical-only color + transition + line-height survive.

## Fix
3 canonical merged + 3 dead blocks deleted + 3 marker comments.

## Test (15 layers)
- 3× rule-head counts.
- 3× cascade-resolved truth.
- 1 NEGATIVE-cluster (5 dead canonical .nav-key decls defended).
- bd-1982d4 preservation pin (.nav-item.active .nav-icon count==2).
- 3 marker pins + bd-360942 sibling pin.

## Pattern (m) preflight skipped 4 of 7 candidates
- `.ws-split--v > .ws-split-handle::after` (intentional split: L11412 compound shares base decls, L11433 standalone adds variant-specific decls).
- `.node-detail-card-title` (L2683 3-selector compound-tail).
- `.node-agent-row` (L2676 4-selector compound-tail).
- `.node-agent-main` (L2668 4-selector compound-tail).

## Design-shift note on .nav-key opacity 0→0.5
Canonical's pattern was hidden-until-hover (opacity:0 + transition + companion `.nav-item:hover .nav-key, .nav-item.active .nav-key { opacity: 0.7; }` rule). Late shifted to visible-at-0.5 with brighter 0.8 hover. The cascade-resolved visible truth: 0.5 default + 0.8 hover (NOT canonical's hidden-until-hover). The canonical 0.7-on-active companion rule is overridden separately by the 0.8 hover-only rule.

Marker explicitly documents that the hidden-until-hover affordance is dead, and that the `.nav-item:hover .nav-key` separate rule remains in place because it operates on a separately-cascaded canonical compound rule.

## Pattern combination
- 2× strictly-additive.
- 1× silent-override mixed survival with explicit **pattern (v) sub-variant** on opacity (cascade-dead-elaborate-canonical — canonical's hidden-until-hover affordance was the elaborate one; late's visible-at-0.5 simpler form is the cascade winner).

Pattern (m) preflight correctly identified that the post-bd-360942 audit pool was dominated by intentional split / compound-tail patterns. Smaller cycle, cleaner safety margin.

## Diff summary
- Files touched:
  - `crates/caco-web/static/style.css` -- 3 canonical merged; 3 dead blocks deleted; 3 marker comments.
  - `crates/caco-web/src/tests.rs` -- bd-debc7c regression test.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*`.
- Net pass: 544 -> 545; 0 failures.

## Operator-takeaway
35 cycles, 78 wins. Pattern (m) preflight reaching saturation point — most remaining duplicates are intentional splits or compound-tails. Catalog: 20 entries (a-q + s + t + u + v + w).
