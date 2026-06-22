# Session summary — bd-46f5ea: style.css 3x nav family selectors triple-paired dedup incl var-equivalent (bd-2e59b2 sibling)

## Goal
Continue bd-2e59b2/bd-3cb20a/.../bd-f479b0 dup-block audit. Triple-paired cycle in nav family.

## Bead
- `bd-46f5ea`

## The six blocks
1. `.nav-list` (additive): canonical (list-style+padding+flex) + late (scroll props).
2. `.nav-item` (silent-override on transition): canonical 11-prop incl `transition: all var(--transition)`; late narrow `background/color/border-color 0.15s` wins.
3. `.nav-item:focus-visible` (var-equivalent dup): canonical uses var(--accent), late uses var(--nord8); confirmed `--accent: var(--nord8);` at line 78. Identical visible color.

## False-positive caught (pattern m)
Originally scoped 4x; `.nav-item.active .nav-icon` looked like adjacent-duplicate but was actually 1 compound rule (`.nav-item:hover .nav-icon, .nav-item.active .nav-icon { opacity: 1; }`) + 1 standalone (`{ filter: drop-shadow(...); }`). Reverted; bead title 4x → 3x. Same awk-scan false-positive root cause as bd-dfbeca `.ws-split--v`.

## NEW SUB-PATTERN VARIANT — var-equivalent dup
Custom-property indirection chain `--accent: var(--nord8);` makes `var(--accent)` and `var(--nord8)` resolve to the same #88c0d0 token. Late `.nav-item:focus-visible { outline: 2px solid var(--nord8); ... }` is byte-identical-equivalent to canonical's `var(--accent)` version. Pure dead block; visible color identical. Sub-variant of bd-c4e2ee byte-identical-subset pattern.

## Fix
3 canonical merged + 3 late deleted + 3 marker comments. bd-c4e2ee sibling-test intact (2-standalone assertion preserved).

## Test (11 layers)
- 3x rule-head counts.
- 3x baseline+promoted/preserved.
- 1 NEGATIVE (dead all-transition).
- 3 marker pins + bd-2e59b2 sibling pin.

## Pattern combination
1x additive (bd-f0393a) + 1x silent-override (bd-3dfff5/bd-238cea) + 1x var-equivalent byte-identical-subset (NEW sub-variant) + awk-scan false-positive detection (bd-dfbeca pattern m).

## Diff summary
- Files touched:
  - `crates/caco-web/static/style.css` -- 3 canonical merged; 3 dead blocks deleted; 3 marker comments.
  - `crates/caco-web/src/tests.rs` -- bd-46f5ea regression test.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*`.
- Net pass: 529 -> 530; 0 failures.

## Operator-takeaway
20 cycles, 63 wins. var-equivalent dup is a new sub-variant catalog entry. Pattern m (awk false-positive detection) continues paying dividends; caught a second case here.
