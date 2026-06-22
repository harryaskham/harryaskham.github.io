# Session summary — bd-20aee5: style.css .view ×2 dedup + dead viewIn @keyframes removal (bd-b2b739 sibling, orphan-keyframe cleanup)

## Goal

Continue the bd-b2b739 / bd-3dfff5 / bd-39161c /
bd-c4e2ee / bd-0cb4d9 / bd-f479b0 dead-rule /
dup-block audit. `.view` appeared **two** times with
cascade analysis revealing a silent-override that
left an `@keyframes` definition fully orphaned.

## Bead(s)

- `bd-20aee5` — [caco-web] style.css .view ×2 dedup + dead viewIn @keyframes removal

## The duplicate + orphan keyframe

### Line 913 canonical structural block
```css
.view {
    display: none;
    padding: 28px 36px;
    flex: 1;
    /* ... */
    animation: viewIn 0.32s cubic-bezier(0.22, 1, 0.36, 1);
}

@keyframes viewIn {
    from { opacity: 0; transform: translateY(6px) scale(0.995); }
    to { opacity: 1; transform: translateY(0) scale(1); }
}
```

### Line 7424 late silent-override
```css
/* Smooth view transitions */
.view {
    animation: view-fade 0.15s ease-out;
}
@keyframes view-fade {
    from { opacity: 0.7; }
    to { opacity: 1; }
}
```

## Silent-override + orphan-keyframe diagnosis

Same specificity for both `.view` blocks → later
wins. `animation: viewIn 0.32s cubic-bezier(...)`
from line 913 silently overridden by `animation:
view-fade 0.15s ease-out` from line 7424. The
sophisticated entry animation (translateY + scale)
has been silently replaced with a simple opacity
fade.

**Orphan consequence**: with `viewIn` animation
declaration overridden, the `@keyframes viewIn`
block at line 924 has ZERO references in the entire
codebase (verified by cross-codebase grep for both
`.css` and `.js` files).

## Fix

Three-part cleanup preserving current visible
behavior:

1. Update line 913 block: promote `animation:
   view-fade 0.15s ease-out;` into the canonical
   structural block.
2. Delete dead `@keyframes viewIn` (orphan, zero
   references after step 1).
3. Delete late dup `.view { animation: view-fade
   ...; }` block at line 7424.

Three marker comments document each deletion.

## Test design (7 layers)

1. **Exactly 1 top-level `.view {` rule head** (was
   2) via line-prefix filter.
2. **Merged block contains promoted animation**
   declaration + structural baseline (display /
   padding / overflow-y).
3. **NEGATIVE**: dead `animation: viewIn 0.32s`
   declaration must remain removed.
4. **NEGATIVE**: dead `@keyframes viewIn` block must
   remain removed (orphan after override).
5. **@keyframes view-fade** definition remains
   (active animation source).
6. **Three replacement marker comments** document
   each deletion.
7. **bd-b2b739 sibling pattern presence pin**.

## Lesson learned

**Marker-text NEGATIVE-assertion conflict**: initial
marker comments contained the literal substrings
the test was NEGATIVE-asserting against (e.g.
`animation: viewIn 0.32s cubic-bezier(...)` for
documentation clarity). The test then matched the
marker text and failed.

Resolved by rewording markers to descriptive prose
("the prior viewIn animation", "viewIn @keyframes
block") rather than embedded literal substrings.

**Pattern for future sibling cycles**: when
NEGATIVE-asserting a dead substring, the assertion
forbids that substring from appearing anywhere in
the file, including marker comments — phrase
markers descriptively rather than embedding the
exact removed code.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/style.css` -- 1 block merged, 1 dead keyframe deleted, 1 dead dup block deleted, 3 marker comments added.
  - `crates/caco-web/src/tests.rs` -- new bd-20aee5 regression test with 7 assertion layers including TWO NEGATIVE assertions (silent-override + orphan-keyframe).
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 516 -> 517; 0 failures.

## Operator-takeaway

style.css continues shrinking. The bd-f479b0 ->
bd-0cb4d9 -> bd-c4e2ee -> bd-39161c -> bd-3dfff5 ->
bd-b2b739 -> bd-20aee5 family chain now demonstrates
dedup patterns for: (a) shared late-shadowing
additions, (b) pure byte-identical duplicates, (c)
compound-vs-standalone selector disambiguation, (d)
silent-override NEGATIVE assertion, (e) visual-
composition preservation, **(f) orphan-@keyframes
cleanup paired with silent-override dedup, (g)
NEGATIVE-assertion marker-text conflict resolution
via descriptive markers**.
