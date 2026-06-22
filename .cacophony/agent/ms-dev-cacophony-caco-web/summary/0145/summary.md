# Session summary — bd-94ee2d: remove 6 dead CSS utility classes

## Goal
Pattern (m) dead-utility-class dedup: 6 classes defined but zero external references.

## Bead
- `bd-94ee2d`

## Audit
- Audited 1012 CSS classes in style.css; found 116 candidates with no class="..." HTML/JS reference.
- Refined: removed candidates with dynamic template-literal generators (e.g. `chip-state-${X}`, `bead-row-${tone}`) and JS `document.querySelector('.app-header')` patterns.
- Final 6 truly-dead utility classes: 4 `.aspect-*` + 2 `.clamp-*`.

## Fix
Removed all 6 declarations + orphan `/* Aspect ratio utilities */` section comment.

## Kept
Other utility framework classes (`.gap-*`, `.block`, `.flex`, `.grid`, `.inline-flex`, `.inline-block`, `.hidden`) — may be future-used.

## Regression test (~80 lines)
- Definition check: each of 6 classes NOT defined as `.CLS {`.
- External-reference check: 22 scan files; 5 probes each (`"CLS"`, `'CLS'`, ` CLS `, ` CLS"`, ` CLS'`) all absent.

## Operator-visible effect
- Identical visual rendering.
- ~250 bytes CSS savings.
- Cleaner utility section.

## Diff summary
- `crates/caco-web/static/style.css` -- 6 declarations + section comment removed.
- `crates/caco-web/src/tests.rs` -- new bd-94ee2d forward-guard (~80 lines).
- Net pass: 562 -> 563; 0 failures.

## Operator-takeaway
53 cycles, 96 wins. Pattern (m) dead-utility-class dedup. Pattern catalog: 22 entries.
