# Session summary — bd-0cb4d9: style.css body rule dedup (bd-f479b0 sibling)

## Goal

Continue the bd-f479b0 dead-rule / duplicate-block
audit of style.css (~295KB). The `body { ... }`
selector appeared **four** times.

## Bead(s)

- `bd-0cb4d9` — [caco-web] style.css body rule dedup: merge 4 duplicate body blocks into 1

## The four body blocks

| Line | Role | Verdict |
|---|---|---|
| 138 | Canonical baseline (font, ambient glow, font-smoothing) | Merge target |
| 7621 | Late `background: linear-gradient(...)` + `min-height: 100vh/dvh` | Merge into 138 (was shadowing the glow) |
| 7888 | Pure byte-identical duplicate of 138's font-smoothing triplet | DELETE (pure dup) |
| 7906 | One-off `min-width: 320px` | Merge into 138 |

## The subtle shadowing bug

The late block at line 7621 set `background:` (the
shorthand), which **resets `background-image`** —
silently dropping the canonical block's three-layer
ambient glow `background-image: radial-gradient(...)`
declarations. Users have been seeing only the
linear-gradient for an unknown duration.

This is longstanding visible behavior, so the merge
preserves it as the post-refactor truth and leaves a
marker comment explaining how to restore the ambient
glow design intent (via multi-background combination)
as a future explicit decision rather than a refactor
side-effect.

## Fix

Merged all four blocks into ONE canonical block at
line 138:

```css
body {
    font-family: var(--font-sans);
    /* bd-0cb4d9: merged background -- the prior layered ambient glow
       background-image (3 radial-gradients) was silently shadowed ... */
    background: linear-gradient(180deg, var(--bg-primary) 0%, var(--bg-secondary) 100%);
    color: var(--text-primary);
    font-size: 14px;
    line-height: 1.55;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    text-rendering: optimizeLegibility;
    min-height: 100vh;
    /* bd-0bf8cb: dvh tracks the dynamic viewport on iOS Safari ... */
    min-height: 100dvh;
    /* bd-0cb4d9: min-width merged from former late ... */
    min-width: 320px;
}
```

Each deleted late block replaced with a marker
comment documenting WHY it was removed — defense
against a future contributor re-adding a parallel
late block without noticing the canonical merged
target.

## Test design (8 layers)

1. **Exactly ONE top-level `body {` rule head** —
   line-prefix filter per bd-f479b0 (was 4 before
   merge); indented `body {` inside @media blocks
   still allowed.
2. **Linear-gradient preserved** in merged block
   (current visible truth).
3. **`min-height: 100vh` AND `min-height: 100dvh`**
   preserved (bd-0bf8cb iOS Safari pair).
4. **`min-width: 320px`** preserved.
5. **Font / font-smoothing / text-rendering quartet**
   preserved.
6. **bd-0bf8cb comment marker** preserved (UX
   context).
7. **Three replacement marker comments** document
   each deletion (defense against re-introduction).
8. **bd-f479b0 sibling pattern presence pin**
   (`.inline-image` merged-canonical block marker).

## Sibling test impact

The bd-0bf8cb dvh-pair test transiently failed
because a marker comment string accidentally
substring-matched `height: 100vh` one extra time
(it said "min-height: 100vh/100dvh pair"). Reworded
the marker to "dynamic-viewport fallback pair"
which doesn't contain the substring.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/style.css` -- 4 body blocks merged into 1; 3 late blocks replaced with marker comments; ~30 lines removed, ~20 lines comments added.
  - `crates/caco-web/src/tests.rs` -- new bd-0cb4d9 regression test with 8 assertion layers.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 511 -> 512; 0 failures.

## Operator-takeaway

style.css is smaller and has one canonical `body {}`
block instead of four divergent ones. Future
contributors editing body-level styling only need to
edit one place. The latent shadowing bug (line 7621
silently dropping the ambient glow design intent) is
now explicitly documented as a marker comment so the
design intent can be revisited deliberately. Visible
output is byte-identical (pure no-visible-change
refactor).
