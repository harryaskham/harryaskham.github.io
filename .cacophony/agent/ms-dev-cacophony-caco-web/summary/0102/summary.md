# Session summary — bd-341650: style.css .view-header (silent-override) + .view-controls (byte-identical) paired dedup (bd-238cea sibling)

## Goal

Continue the bd-238cea / bd-f0393a / bd-20aee5 /
bd-b2b739 / bd-3dfff5 / bd-39161c / bd-c4e2ee /
bd-0cb4d9 / bd-f479b0 dead-rule / dup-block audit.
The view-chrome family adjacent to bd-20aee5's
`.view` cycle had TWO duplicate-selector pairs.

## Bead(s)

- `bd-341650` — [caco-web] style.css .view-header + .view-controls paired dedup

## The four blocks

### `.view-header` pair (silent-override)
- Line 941 canonical: display:flex, align-items, justify-content, **margin-bottom: 24px**, flex-shrink, flex-wrap, gap.
- Line 7439 late refinement: border-bottom, padding-bottom, **margin-bottom: 16px**.

Cascade: same specificity, later wins. `margin-bottom: 24px` silently overridden to `16px`. `border-bottom` and `padding-bottom` survive.

### `.view-controls` pair (pure byte-identical dup)
- Line 971 canonical: display:flex, align-items, gap, flex-wrap.
- Line 8029 late: SAME EXACT DECLARATIONS.

Pure byte-identical duplicate. Zero functional effect.

## Fix (paired-selector atomic merge combining BOTH patterns)

1. **`.view-header`**: update canonical block at 941 — promote `margin-bottom: 16px` (cascade-resolved truth) + ADD surviving `border-bottom: 1px solid var(--border);` + `padding-bottom: 12px;`. Delete late dup at 7439.
2. **`.view-controls`**: keep canonical block at 971 unchanged (it's already the visible truth). Delete pure-dup at 8029.

Two consolidated marker comments document the merges.

## Test design (7 layers)

1. Exactly 1 `.view-header {` rule head (was 2).
2. Exactly 1 `.view-controls {` rule head (was 2).
3. Merged `.view-header` block preserves structural baseline + promoted margin-bottom:16px + promoted border-bottom + padding-bottom.
4. NEGATIVE: dead `margin-bottom: 24px;` must remain removed from `.view-header` block (leading-whitespace anchor per bd-238cea lesson).
5. Merged `.view-controls` block preserves all baseline properties.
6. Two replacement marker comments document each merge.
7. bd-238cea sibling pattern presence pin.

## Pattern combination demonstrated

This cycle combines BOTH dedup families in one
atomic merge:
- **Silent-override** (`.view-header`)
- **Byte-identical duplicate** (`.view-controls`)

Per bd-238cea's paired-selector pattern, both
selectors are deduped together so a single test
guards the visible-composition cross-selector. The
leading-whitespace anchor lesson from bd-238cea is
applied directly (anchor `\n    margin-bottom:
24px;` rather than substring `margin-bottom: 24px;`
to avoid potential false positives elsewhere).

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/style.css` -- 1 silent-override merge + 1 byte-identical dup deletion + 4 marker comments added.
  - `crates/caco-web/src/tests.rs` -- new bd-341650 regression test with 7 assertion layers including 1 NEGATIVE bounded-window assertion with leading-whitespace anchoring.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 519 -> 520; 0 failures.

## Operator-takeaway

style.css continues shrinking. The bd-f479b0 ->
bd-0cb4d9 -> bd-c4e2ee -> bd-39161c -> bd-3dfff5 ->
bd-b2b739 -> bd-20aee5 -> bd-f0393a -> bd-238cea ->
bd-341650 family chain now demonstrates dedup
patterns for: (a) shared late-shadowing additions,
(b) pure byte-identical duplicates, (c) compound-vs-
standalone selector disambiguation, (d) silent-
override NEGATIVE assertion, (e) visual-composition
preservation, (f) orphan-`@keyframes` cleanup, (g)
NEGATIVE-assertion marker-text conflict resolution,
(h) strictly-additive consolidation, (i) paired-
selector atomic merge, (j) NEGATIVE-assertion
leading-whitespace anchoring for property-name
substring disambiguation, **(k) mixed-pattern paired
merge combining silent-override + byte-identical
dup in one atomic cycle**.

10 cycles, 53 wins compounding this session.
