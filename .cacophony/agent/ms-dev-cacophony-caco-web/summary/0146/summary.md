# Session summary — bd-f0ef44: remove dead --accent-strong CSS var

## Goal
Pattern (m) dead-css-var dedup: 1 declared CSS custom property with zero consumers.

## Bead
- `bd-f0ef44`

## Audit
- 5 CSS files scanned; 97 unique declared `--vars`.
- Per-var consumer count: var(), setProperty, getPropertyValue, inline style="--N:", template-literal style.
- Yield: 2 candidates (--accent-strong, --z-base).
- --accent-strong: orphan from removed accent system; deleted.
- --z-base: kept (documented z-index stratification token, future use intended).

## Fix
Removed `--accent-strong: #8fbcbb;` from workspace.css :root.

## Regression test (~80 lines)
- 23 scan files.
- Per-file: no `var(--accent-strong)`, no `setProperty('--accent-strong')`, no `getPropertyValue('--accent-strong')`.
- workspace.css: comment-stripped redeclaration check.

## Operator-visible effect
- Identical visual rendering.
- ~30 bytes CSS savings.
- Cleaner workspace.css :root.

## Diff summary
- `crates/caco-web/static/workspace.css` -- 1 line removed; receipt comment added.
- `crates/caco-web/src/tests.rs` -- new bd-f0ef44 forward-guard (~80 lines).
- Net pass: 563 -> 564; 0 failures.

## Operator-takeaway
54 cycles, 97 wins. Pattern (m) dead-css-var dedup. Pattern catalog: 22 entries.
