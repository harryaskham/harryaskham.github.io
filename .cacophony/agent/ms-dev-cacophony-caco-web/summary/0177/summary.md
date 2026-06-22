# Session summary — bd-922835: 7 border-radius literals → var()

## Goal
Complete the radius-token convention started by bd-1232b8 (--radius-pill).

## Bead
- `bd-922835`

## Audit
- 6 raw `border-radius: 8px;` (--radius) + 1 raw `border-radius: 16px;` (--radius-xl).
- 1 asymmetric preserved.

## Fix
- sed-replace both literals to var() form.

## Test fallout
- bd-a18fda .context-menu forward-guard updated to expect var(--radius).

## Why
- Behavior preserved; single source of truth; completes radius-token convention.

## Regression test (~40 lines)
- Single/multi-token discrimination via split_whitespace().count().

## Operator-visible effect
- None. Pure refactor.

## Diff summary
- `crates/caco-web/static/style.css` -- 7 selector bodies use var().
- `crates/caco-web/src/tests.rs` -- new bd-922835 forward-guard + 1 updated bd-a18fda assertion.
- Net pass: 592 -> 593; 0 failures.

## Operator-takeaway
85 cycles, 127 wins. Pattern (q)/(r) extension. Pattern catalog: 26 entries.
