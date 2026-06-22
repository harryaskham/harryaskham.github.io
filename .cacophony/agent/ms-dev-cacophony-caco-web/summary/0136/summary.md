# Session summary — bd-17c5b8: orphan-ID-reference test (pattern x #7)

## Goal
Pattern (x) seventh-instance: orphan-ID-reference audit + dead-code prune.

## Bead
- `bd-17c5b8`

## Audit results
- 226 ID references via getElementById/el/querySelector across 19 .js files.
- 289 ID definitions (literal `id="..."` + dynamic `.id = '...'`).
- After investigation: **1 genuinely orphan** = `bead-filter-priority` (refactor-leftover dead clear-handler line in app.js:7114).

## Fix
- Removed the dead `el('bead-filter-priority')` reference.

## Debugging journey
First pass found 11 false-positive "orphans" (`a`, `div`, `span`, etc). Cause: `workspace-chat-pane.js` shadows the global `el(id)` helper with `el(tag, props, children)` jsx-like DOM constructor — first arg there is an HTML tag name, not an ID.

**Fix**: disambiguate by requiring the closing quote of the `el()` arg to be IMMEDIATELY followed by `)` (single-argument call). So `el('foo')` matches but `el('foo', { class: '...' })` does not.

## NEW pattern (x) sub-variant: "function-call-arity-disambiguation"
When the same identifier is overloaded across files (e.g. global `el(id)` vs jsx-like `el(tag, props, children)`), the audit must constrain matches by argument arity (e.g. require `)` immediately after the first arg's closing quote for single-arg calls).

## Pattern (x) generalization (7 audit categories now)
| # | Bead | Category | Finds |
|---|------|----------|-------|
| 1 | bd-91616b | CSS custom properties | 2 prunes |
| 2 | bd-a753e2 | @keyframes | 0 (clean) |
| 3 | bd-cf15b7 | ARIA idrefs | 1 a11y fix |
| 4 | bd-f54973 | button-type integrity | 129 fixes |
| 5 | bd-9e60ac | JS classList integrity | 6 allowlisted |
| 6 | bd-00ec81 | Dynamic-img alt-integrity | 1 a11y fix |
| 7 | bd-17c5b8 | Orphan-ID-reference | 1 dead-code prune |

## Pattern (x) sub-variants now catalogued
- declarative-vs-consumer mismatch (orphan defs).
- consumer-vs-definition (orphan refs) — bd-17c5b8.
- conjoined-definition-space (multi-source) — bd-9e60ac, bd-17c5b8.
- DOM-construction-vs-attribute-init — bd-00ec81.
- function-call-arity-disambiguation — bd-17c5b8 (NEW).

## Operator-visible effect
- Refactor-leftover dead-code lines fail CI with file:line guidance.
- Pattern catches `getElementById`/`el`/`querySelector` references where the element is never created.

## Diff summary
- `crates/caco-web/static/app.js` -- 1 dead-code prune.
- `crates/caco-web/src/tests.rs` -- new bd-17c5b8 regression test (~120 lines).
- Net pass: 553 -> 554; 0 failures.

## Operator-takeaway
44 cycles, 87 wins. Pattern (x) at 7 audit categories with 5 sub-variants. Catalog: 21 entries.
