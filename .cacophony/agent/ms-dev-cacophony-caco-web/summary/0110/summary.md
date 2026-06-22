# Session summary — bd-3cb20a: style.css .quick-bead-chip (silent-override) + :hover (subset-dup) paired dedup (bd-da325e sibling)

## Goal
Continue style.css dup-block audit chain.

## Bead
- `bd-3cb20a`

## The four blocks
1. `.quick-bead-chip` (silent-override on transition): canonical 10-prop baseline incl `transition: all var(--transition)`; late had `transition: background 0.15s, border-color 0.15s` only. Late narrow-scope transition wins.
2. `.quick-bead-chip:hover` (byte-identical SUBSET dup): canonical 4 declarations; late 2 already-present declarations. Pure dead block.

## Fix
2 canonical merged + 2 late deleted + 2 marker comments (combined into 1 marker for both deletions in the same late block).

## Test (7 layers)
1-2. 1 rule head each.
3. .quick-bead-chip baseline + promoted narrow transition.
4. NEGATIVE: dead `transition: all var(--transition);` removed.
5. .quick-bead-chip:hover 4-declaration superset preserved.
6. 2 marker pins.
7. bd-da325e sibling pin.

## Pattern combination
Silent-override (bd-3dfff5/bd-238cea) + byte-identical subset-dup (bd-c4e2ee/bd-f0d094) in one paired cycle.

## Operator workflow note
During bd-da325e close attempt, beads primary entered restart maintenance window (~5 min). Code applied with placeholder bd-de9e21 ID, awaited primary return, then create call deduped to existing bd-3cb20a (filed during a previous attempt that succeeded despite error envelope). Sed pass updated all references; tests still pass.

## Diff summary
- Files touched:
  - `crates/caco-web/static/style.css` -- 2 canonical merged; 1 combined dead block deleted; 2 marker comments.
  - `crates/caco-web/src/tests.rs` -- bd-3cb20a regression test (7 layers).
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*`.
- Net pass: 527 -> 528; 0 failures.

## Operator-takeaway
18 cycles, 61 wins. New ops lesson: beads primary maintenance during close attempt may still actually close the bead before the error envelope arrives; idempotent create operations dedupe to existing IDs.
