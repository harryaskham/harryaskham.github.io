# Session summary — bd-00ec81: dynamic-img alt-integrity test (pattern x #6)

## Goal
A11y bug fix + Pattern (x) sixth-instance for dynamic-img alt-integrity.

## Bead
- `bd-00ec81`

## Audit results
- 12 `<img>` references in static html/js.
- 10 literal `<img ...>` tags in JS template literals — ALL have `alt="${escapeAttr(...)}"`.
- 2 in JS comments (false positives).
- **1 dynamic `Image()` at kitty-graphics.js:449 — was missing alt**.

## Fix
- `kitty-graphics.js:449`: added `img.alt = ''` (decorative kitty graphic — screen readers should skip).

## Regression test (~115 lines)
Walks creation patterns (`new Image(`, `new Img(`, `createElement('img')`, `createElement("img")`) across 16 .js files. For each:
1. Backward-look 80 chars to capture LHS variable identifier.
2. Forward-look 600 chars (~25 lines) for `<varname>.alt` reference OR generic `.alt =` if var unknown.

Failure message: `<file>:<line> -> pattern, var=name` with remediation guidance.

## Pattern (x) generalization (6 audit categories now)
| # | Bead | Category | Finds |
|---|------|----------|-------|
| 1 | bd-91616b | CSS custom properties | 2 prunes |
| 2 | bd-a753e2 | @keyframes | 0 (clean) |
| 3 | bd-cf15b7 | ARIA idrefs | 1 a11y fix |
| 4 | bd-f54973 | button-type integrity | 129 fixes |
| 5 | bd-9e60ac | JS classList integrity | 6 allowlisted |
| 6 | bd-00ec81 | Dynamic-img alt-integrity | 1 a11y fix |

## NEW pattern (x) sub-variant: "DOM-construction-vs-attribute-init"
Demonstrates the pattern targets not just declarative-vs-consumer mismatches (CSS vars, @keyframes, ARIA idrefs, classList classes, button types) but also **DOM-construction-vs-attribute-init pairs**.

The audit shape: scan for construction-call X (`new Image()`, etc), walk forward N chars, require attribute-init Y (`.alt = `) on the constructed object.

## Operator-visible effect
- Screen readers skip kitty graphics instead of announcing blob URLs.
- Future dev adding `new Image()` without setting `.alt` fails CI with file:line remediation guidance.

## Diff summary
- `crates/caco-web/static/kitty-graphics.js` -- 1 a11y fix.
- `crates/caco-web/src/tests.rs` -- new bd-00ec81 regression test (~115 lines).
- Net pass: 552 -> 553; 0 failures.

## Operator-takeaway
43 cycles, 86 wins. Pattern (x) at 6 audit categories with new "DOM-construction-vs-attribute-init" sub-variant. Pattern catalog: 21 entries.
