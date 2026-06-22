# Session summary — bd-1232b8: 26 border-radius 999px → var(--radius-pill)

## Goal
DRY: consolidate raw `border-radius: 999px;` literals into the established `--radius-pill` custom property.

## Bead
- `bd-1232b8`

## Audit
- 26 raw declarations in selector bodies; 18 existing var usages; consolidation extends to 44.
- 3 comment-block literals (documenting prior states) preserved.

## Fix
- sed-replace `^    border-radius: 999px;$` → `    border-radius: var(--radius-pill);`.
- 3 existing forward-guards (bd-f0d094, bd-3cb20a, bd-ef0924) updated to expect the var form.

## Why
- Behavior preserved; single source of truth; discoverability.

## Regression test (~45 lines)
- Line-by-line walk; reject any selector-body line with literal `999px;` in border-radius.
- Comment-block exclusion via `*` prefix + backtick check.

## Test fallout
- 3 existing forward-guards broken by refactor; surgical update to expect var(--radius-pill).
- 2 ABSENCE-check forward-guards (bd-a6c685) intentionally untouched (still assert literal absence).

## Operator-visible effect
- None. Pure refactor.

## Diff summary
- `crates/caco-web/static/style.css` -- 26 selector bodies use var().
- `crates/caco-web/src/tests.rs` -- new bd-1232b8 forward-guard (~45 lines) + 3 updated assertions.
- Net pass: 591 -> 592; 0 failures.

## Operator-takeaway
84 cycles, 126 wins. Pattern (q) named CSS custom-property for duplicated literals (extending bd-23f35c). Pattern (r): refactor sweeps must update existing literal-checking forward-guards. Pattern catalog: 26 entries.
