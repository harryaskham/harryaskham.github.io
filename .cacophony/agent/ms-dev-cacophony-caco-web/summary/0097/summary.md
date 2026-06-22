# Session summary — bd-3dfff5: style.css textarea ×3 dedup (bd-39161c sibling)

## Goal

Continue the bd-39161c / bd-c4e2ee / bd-0cb4d9 /
bd-f479b0 dead-rule / dup-block audit. The
`textarea` element selector appeared **three** times,
with cascade analysis revealing one silent-override
bug (`font-family: var(--font-mono)` silently dropped
to `inherit`).

## Bead(s)

- `bd-3dfff5` — [caco-web] style.css textarea ×3 dedup -- merge cascade-resolved survivors, drop silent-override font-family bug

## Cascade analysis

| Property | Line 3212 | Line 8398 | Line 8403 (wins) |
|---|---|---|---|
| `resize` | vertical | — | vertical ✓ (same value, no conflict) |
| `width` | 100% | — | — (3212 survives) |
| `font-family` | var(--font-mono) | — | **inherit** ✓ (silent override) |
| `caret-color` | — | var(--frost-2, var(--nord8)) | — (8398 survives) |
| `line-height` | — | — | 1.5 ✓ |
| `min-height` | — | — | 60px ✓ |

**Silent-override observation**: `font-family:
var(--font-mono)` from line 3212 has been silently
dropped — textareas in the dashboard render in the
inherited body font, not the intended monospace. This
is most likely the correct design decision (general-
purpose textareas should match body font; specific
monospace surfaces apply their own override on child
selectors), so the dedup preserves current visible
behavior and documents the dead intent in a marker
comment.

## Fix

Merged all three blocks into ONE canonical block at
the line 3212 location:

```css
textarea {
    /* bd-3dfff5: merged from three formerly-separate `textarea { ... }`
       blocks. Cascade-resolved properties preserved; the dead
       `font-family: var(--font-mono)` from this block's prior version
       was silently overridden by a later `font-family: inherit`
       declaration -- inherit is the visible truth. */
    resize: vertical;
    width: 100%;
    /* Comfortable typing experience -- merged from the former line 8403 block. */
    line-height: 1.5;
    min-height: 60px;
    font-family: inherit;
    /* Frost caret -- merged from the former line 8398 block. */
    caret-color: var(--frost-2, var(--nord8));
}
```

## Test design (5 layers)

1. **Exactly 1 top-level `textarea {` rule head**
   (was 3 before merge) via line-prefix filter.
2. **Merged block contains all cascade-resolved
   survivors** (resize, width, line-height,
   min-height, font-family: inherit, caret-color) via
   bounded char-window extraction per bd-57c0f5.
3. **NEGATIVE: dead `font-family: var(--font-mono);`
   declaration must remain removed** — first dedup
   cycle that explicitly NEGATIVE-asserts a silent-
   override bug fix.
4. **Two replacement marker comments** document each
   deletion (defense against re-introduction).
5. **bd-39161c sibling pattern presence pin**
   (broader duplicate-block-merge family
   regression-guard).

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/style.css` -- 3 textarea blocks merged into 1; 2 late blocks replaced with marker comments; ~10 lines net deleted, ~8 lines comments added.
  - `crates/caco-web/src/tests.rs` -- new bd-3dfff5 regression test with 5 assertion layers (first cycle with explicit NEGATIVE assertion for silent-override bug).
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 514 -> 515; 0 failures.

## Operator-takeaway

style.css continues shrinking. The bd-f479b0 ->
bd-0cb4d9 -> bd-c4e2ee -> bd-39161c -> bd-3dfff5
family chain now demonstrates the dedup pattern for:
(a) shared late-shadowing additions, (b) pure
byte-identical duplicates, (c) compound-vs-standalone
selectors needing selector-continuation detection,
**(d) silent-override bug documentation via NEGATIVE
assertion + marker comment preserving the dead design
intent**. Future cycles can target `.choice-card` ×3
(transition-property silent override) and the more
complex `.btn-primary` ×3 (three different design
iterations with overlapping survivors).
