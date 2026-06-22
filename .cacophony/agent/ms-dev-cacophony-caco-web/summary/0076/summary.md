# Session summary — bd-57c0f5: content-visibility:auto on .inbox-card

## Goal

Backlog item: "content-visibility on bead-table rows".
Investigated, and `<tr>` rows have known browser quirks
around column-width measurement under `content-visibility:
auto`. Pivoted to the same primitive on `.inbox-card`,
which is a plain `<div>` with no such caveats.

## Bead(s)

- `bd-57c0f5` — [caco-web] add content-visibility:auto + contain-intrinsic-size:120px to .inbox-card

## Established primitive

style.css already uses the off-viewport-skip primitive on
4 selectors:

| Selector | line | intrinsic-size |
|---|---|---|
| `.feed-entry` | 1882 | 28px |
| `.diff-line` | 4113 | 18px |
| `.log-line` | 6028 | 30px |
| `.notification-item` | 6135 | 80px |

All use the bare-`<length>` form of `contain-intrinsic-size`
(Safari 18.0 baseline; the `auto <length>` keyword form
needs 17.4+). `.inbox-card` was missing the primitive.

## Why .inbox-card is a good candidate

- Rendered via unbounded `items.map(...).join('')` (no
  virtualization).
- Multiple inbox entry kinds — notifications, choices,
  direct messages, broadcasts, speech — accumulate.
- Each card is ~110-140px tall: 16+16 padding + ~22
  header + ~40-60 body + ~16 meta + 12 margin-bottom.
- Plain `<div>`, no focus traps / live regions /
  animations / special interactions. Safe to skip.

For an operator with 200 accumulated inbox entries, only
~5-10 are in viewport at a time. `content-visibility: auto`
means the browser:

- Reserves the 120px slot for layout.
- Skips style/layout/paint of the descendant tree until
  the card scrolls within ~scrollMargin of the viewport.
- Re-flows the card when it intersects (intended cost).

## Fix

```css
.inbox-card {
    /* bd-57c0f5: reserve intrinsic size and skip rendering of off-
       viewport cards. ... */
    content-visibility: auto;
    contain-intrinsic-size: 120px;
    background: linear-gradient(155deg, ...);  /* existing */
    ...
}
```

## Test design (block-scoped + sibling-pin)

1. **Block scoping via brace-depth counter**: extracts the
   `.inbox-card { ... }` body and asserts BOTH primitives
   live within IT (not in some other later selector that
   happens to be in source order).
2. **Total-count regression-guard**: `>= 5` usages (4
   pre-existing + this one), so a future "delete this
   primitive" sweep fails loudly.
3. **Sibling-presence pin**: explicitly require
   `.feed-entry` / `.diff-line` / `.log-line` /
   `.notification-item` selectors remain in style.css.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/style.css` -- 12-line addition (10 comment + 2 declarations) at top of .inbox-card block.
  - `crates/caco-web/src/tests.rs` -- regression test with block-scoping + total-count + sibling-pin layers.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 482 -> 483; 11 pre-existing failures on main unchanged.

## Operator-takeaway

Operators with many accumulated inbox entries now pay
proportional layout/paint cost to **viewport size**
instead of total inbox length. The visible cards render
identically; only the off-screen ones (which the user
isn't looking at anyway) are skipped until they scroll
into view.
