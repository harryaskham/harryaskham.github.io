# Session summary — bd-f0d094: style.css .version-tag (subset-dup) + .workspace-badge (additive) paired dedup (bd-ae85f4 sibling)

## Goal

Continue the bd-ae85f4 / bd-341650 / bd-238cea /
bd-f0393a / bd-20aee5 / bd-b2b739 / bd-3dfff5 /
bd-39161c / bd-c4e2ee / bd-0cb4d9 / bd-f479b0 dead-
rule / dup-block audit. Sidebar/chrome family had
TWO duplicate-selector pairs.

## Bead(s)

- `bd-f0d094` — [caco-web] style.css .version-tag + .workspace-badge paired dedup

## The four blocks

### `.version-tag` pair (byte-identical SUBSET dup)
- Line 333 canonical: 5 declarations (font-size, color, font-family, display:block, margin-top:2px).
- Line 7925 late: 3 declarations (font-size, color, font-family). Pure SUBSET; zero new props, zero overrides.

### `.workspace-badge` pair (strictly-additive merge)
- Line 6577 canonical: 11 declarations (display:inline-flex, gradient bg, border-radius:999px, etc).
- Line 7019 late: `animation: workspace-pop 0.25s ease-out` only. Disjoint.

## Fix

1. **`.version-tag`**: keep canonical (superset truth) unchanged; delete subset dup at 7925.
2. **`.workspace-badge`**: consolidate animation into canonical at 6577; delete additive-only late block at 7019.

`@keyframes workspace-pop` stays (active animation source).

## Test design (7 layers)
1-2. 1 rule head each (was 2 each).
3. `.version-tag` full 5-declaration superset preserved.
4. `.workspace-badge` 11-declaration baseline + consolidated animation.
5. `@keyframes workspace-pop` retained.
6. 2 marker comments.
7. bd-ae85f4 sibling pin.

## Pattern combination

Mixed paired-merge: **byte-identical subset dup** (bd-c4e2ee) + **strictly-additive consolidation** (bd-f0393a) in one atomic cycle.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/style.css` -- 1 canonical merged with consolidated additive prop; 1 dead subset dup deleted; 2 marker comments added.
  - `crates/caco-web/src/tests.rs` -- new bd-f0d094 regression test with 7 assertion layers.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 521 -> 522; 0 failures.

## Operator-takeaway

12 cycles, 55 wins. style.css continues shrinking. Mixed-pattern paired-merge framework scales smoothly across byte-identical subset + additive consolidation in one cycle.
