# Session summary — bd-5d2104: delete dead-cascade duplicate `* {}` scrollbar block

## Goal

Address the "remove duplicate `* { scrollbar-width... }`
at style.css:4392 (dead-cascade)" item from the backlog
in the Critical Context.

## Bead(s)

- `bd-5d2104` — [caco-web] delete dead-cascade global `* { scrollbar-color }` block at style.css:4392

## Before state

Two identical global selectors in style.css with identical
specificity but different scrollbar tints:

```css
/* line 4392 */
* {
    scrollbar-width: thin;
    scrollbar-color: rgba(216, 222, 233, 0.1) transparent;
}

/* line 7064 (~2670 lines later) */
* { scrollbar-width: thin; scrollbar-color: var(--nord3) var(--nord0); }
```

By CSS cascade rules: equal specificity → later rule wins.
The 4392 block was **dead CSS** — its `scrollbar-color`
fully overridden, its `scrollbar-width: thin` a no-op
duplicate. A maintainer trying to tweak the global
scrollbar tint by editing the 4392 rule would see no
effect and have to chase the second selector.

## Verification before edit

Confirmed nothing between 4392 and 7064 mutates global
`scrollbar-*`:

- `.filter-chip-row` at 4561 sets `scrollbar-width: none`
  — scoped, intentional (chip rows hide native scrollbar
  in favor of a mask-based fade affordance from bd-34a693).
- Selector at 7391 sets a scoped scrollbar tint with
  higher specificity than `*`.

Both win against the global rules by specificity, not
source order. Safe to delete the dead block.

## After state

Surviving rule at line 7059 (was 7064 before the deletion
shifted line numbers):

```css
* { scrollbar-width: thin; scrollbar-color: var(--nord3) var(--nord0); }
```

Same theme tokens, same visible behavior. The dashboard
scrollbar tint (—nord3 thumb on —nord0 track) is
unchanged from what users see today.

## Test design

Three invariants:

1. **Exactly ONE bare `* {` at start-of-line** (line-prefix
   filter, so scoped `.foo *` and `:where(*)` aren't
   accidentally counted).
2. **Surviving rule preserves the theme tokens** —
   regression-guard against accidental modification.
3. **Forward-guard against the dead literal value** —
   uses `format!()` concatenation per the bd-5e0030
   defense-in-depth pattern established last cycle.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/style.css` -- 4-line dead-cascade global selector deleted at the old 4392-4395 location.
  - `crates/caco-web/src/tests.rs` -- regression test pins exactly-one bare global, surviving theme-token rule, and forward-guard.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 479 -> 480; 11 pre-existing failures on main unchanged.

## Operator-takeaway

~120 bytes shaved from the stylesheet payload (delivered
on every page load); the visible behavior is unchanged
(scrollbar tint preserved). One less dead-cascade
maintenance trap — a future stylesheet editor changing
the global scrollbar tint won't have to chase a phantom
rule that has no effect.
