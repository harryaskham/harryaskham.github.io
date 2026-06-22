# Session summary — bd-c4e2ee: style.css .nav-item.active .nav-icon dup removal (bd-0cb4d9 sibling)

## Goal

Continue the bd-0cb4d9 / bd-f479b0 dead-rule / dup-
block audit of `style.css`. The `.nav-item.active
.nav-icon` selector appeared **three** times, with
one block being a pure byte-identical duplicate of
an earlier one.

## Bead(s)

- `bd-c4e2ee` — [caco-web] style.css .nav-item.active .nav-icon dup removal

## The three blocks

### Line 432 (canonical compound selector)
```css
.nav-item:hover .nav-icon,
.nav-item.active .nav-icon {
    opacity: 1;
}
```

### Line 436 (canonical drop-shadow)
```css
.nav-item.active .nav-icon {
    filter: drop-shadow(0 0 4px rgba(136, 192, 208, 0.5));
}
```

### Line 7135 (pure duplicate)
```css
/* Subtle glow on active nav item icon */
.nav-item.active .nav-icon {
    filter: drop-shadow(0 0 4px rgba(136, 192, 208, 0.5));
}
```

Byte-identical to line 436's filter declaration; the
introductory comment just restates the line 436
block's intent. Pure dead repeat.

## Fix

Deleted line 7135's dup block. Replaced with marker
comment documenting WHY (defense against re-
introduction).

## Test design (5 layers)

1. Exactly TWO top-level `.nav-item.active .nav-icon {`
   rule heads (was 3 before dedup) via line-prefix
   filter per bd-f479b0 / bd-0cb4d9 pattern.
2. Canonical compound `.nav-item:hover .nav-icon,
   .nav-item.active .nav-icon { opacity: 1; }` block
   remains (survivor pin).
3. Canonical drop-shadow `.nav-item.active .nav-icon
   { filter: drop-shadow(...); }` block remains
   (unique-property survivor pin).
4. Replacement marker comment documents the deletion
   (defense against re-introduction).
5. bd-0cb4d9 sibling pattern presence pin (merged-
   canonical body block marker) -- broader
   duplicate-block-merge family regression-guard.

## Lesson learned

Literal `{` / `}` inside `assert!` message format
strings must be doubled. Initial compile failed
because two messages embedded raw CSS-rule braces
(e.g., `{ opacity: 1; }`); fixed by doubling to
`{{` / `}}`. Per critical context: `assert!` literal
`{`/`}` must be doubled.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/style.css` -- 1 dead block deleted, 1 marker comment added.
  - `crates/caco-web/src/tests.rs` -- new bd-c4e2ee regression test with 5 assertion layers.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 512 -> 513; 0 failures.

## Operator-takeaway

style.css continues shrinking. Three more dup-block
candidates remain in scope for future sibling cycles
(`.data-table thead th` ×3, `.btn-primary` ×3,
`textarea` ×3 etc.). The duplicate-block-merge family
guard chain (bd-f479b0 -> bd-0cb4d9 -> bd-c4e2ee)
documents the survivor pattern so the dedup pass can
continue safely.
