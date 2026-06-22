# Session summary — bd-23f35c: --font-code consolidation

## Goal
DRY: consolidate 5 duplicate FiraCode Nerd Font literals into a named --font-code custom property.

## Bead
- `bd-23f35c`

## Audit
- font-family inventory: 5 literal duplicates of `'Fira Code', 'FiraCode Nerd Font', 'SF Mono', monospace`.
- Differs from --font-mono (JetBrains Mono primary) -- code contexts want Fira Code first for Nerd Font glyphs.

## Fix
- :root + --font-code with rationale comment.
- 5 selector bodies (agent-tty, kbd, code, pre, samp/var): literal -> var(--font-code).

## Why
- Single source of truth; future code-font changes touch 1 declaration.
- Discoverability via :root convention.
- Behavior preserved.

## Regression test (~35 lines)
- --font-code: declared exactly once.
- 'FiraCode Nerd Font' literal appears exactly once (in the declaration).

## Operator-visible effect
- None. Pure refactor.

## Diff summary
- `crates/caco-web/static/style.css` -- +1 :root declaration; 5 selector bodies use var().
- `crates/caco-web/src/tests.rs` -- new bd-23f35c forward-guard (~35 lines).
- Net pass: 590 -> 591; 0 failures.

## Operator-takeaway
83 cycles, 125 wins. Pattern (q) named CSS custom-property for duplicated literals. Pattern catalog: 25 entries.
