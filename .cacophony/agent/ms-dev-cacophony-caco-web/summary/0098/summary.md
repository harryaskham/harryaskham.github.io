# Session summary — bd-b2b739: style.css search-cancel-button middle dead-override dedup (bd-3dfff5 sibling)

## Goal

Continue the bd-3dfff5 / bd-39161c / bd-c4e2ee /
bd-0cb4d9 / bd-f479b0 dead-rule / dup-block audit.
`input[type="search"]::-webkit-search-cancel-button`
selector appeared **three** times, with the MIDDLE
block fully dead (every property overridden by the
canonical mask-based block later in the file).

## Bead(s)

- `bd-b2b739` — [caco-web] style.css input[type='search']::-webkit-search-cancel-button middle dead-override dedup

## The three blocks

| Line | Design | Verdict |
|---|---|---|
| 3198 | Early opacity-fade (opacity:0.65, transition:opacity, hover→opacity:1) | SURVIVOR (opacity + transition + hover all survive) |
| 7519 | Middle override (height:12px, width:12px, icon-URL bg) | DEAD (every prop overridden by 8690) |
| 8690 | Canonical mask-based (appearance, mask-bg, hover→aurora-red) | SURVIVOR (mask + hover-red) |

## Cascade analysis for line 7519

| Property | Line 7519 | Line 8690 (wins) |
|---|---|---|
| `-webkit-appearance` | none | none ✓ |
| `height` | 12px | 12px ✓ (same value) |
| `width` | 12px | 12px ✓ (same value) |
| `background` | url(...d8dee9 circle...) | **var(--text-faint, #6b7280)** ✓ |
| `cursor` | pointer | pointer ✓ |

Line 7519 contributes ZERO unique surviving
properties. Pure dead repeat.

## Visual composition observation

Today the search-cancel-button visually composes
properties from BOTH surviving blocks:

- **Base state**: mask-recoloured X (gray, from 8690)
  at opacity 0.65 (from 3198).
- **Hover state**: opacity:1 (from 3198's hover) AND
  background:aurora-red (from 8690's hover) — the
  cancel button fades-in AND turns red on hover.

The composition is preserved by removing only the
fully-dead middle block.

## Fix

Deleted line 7519's dead block; replaced with marker
comment documenting WHY (defense against
re-introduction).

## Test design (6 layers)

1. **Exactly 2 top-level rule heads** (was 3) via
   line-prefix filter.
2. **Early opacity-fade survivor pin**:
   `opacity: 0.65` + `transition: opacity
   var(--transition)` declarations preserved.
3. **Late mask-based survivor pin**:
   `background: var(--text-faint, #6b7280)` +
   `-webkit-mask:` declarations preserved.
4. **BOTH companion `:hover` blocks survive** —
   distinct visual effects compose (opacity:1 hover
   from early design + aurora-red bg hover from late
   design).
5. **Replacement marker comment** documents the
   deletion (defense against re-introduction).
6. **bd-3dfff5 sibling pattern presence pin**
   (broader duplicate-block-merge family
   regression-guard).

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/style.css` -- 1 dead block deleted, 1 marker comment added.
  - `crates/caco-web/src/tests.rs` -- new bd-b2b739 regression test with 6 assertion layers including the visual-composition guard (both :hover blocks must survive distinct effects).
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 515 -> 516; 0 failures.

## Operator-takeaway

style.css continues shrinking. This cycle introduces
a new pattern in the family chain — **visual-
composition preservation**: when multiple non-dead
blocks contribute distinct visible effects that
compose (here, opacity-fade + red-tint on hover), the
test asserts BOTH companion blocks survive so the
composition isn't accidentally broken by a future
sibling cycle. The bd-f479b0 -> bd-0cb4d9 -> bd-c4e2ee
-> bd-39161c -> bd-3dfff5 -> bd-b2b739 chain now
demonstrates dedup patterns for: (a) shared late-
shadowing additions, (b) pure byte-identical
duplicates, (c) compound-vs-standalone selector
disambiguation, (d) silent-override NEGATIVE
assertion, (e) **visual-composition preservation**.
