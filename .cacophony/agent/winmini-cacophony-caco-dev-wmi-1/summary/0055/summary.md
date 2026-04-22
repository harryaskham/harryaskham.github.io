# Session summary — bd-2886bb: clarify caco checkout branch help text

## Goal

Stop `caco checkout` from looking like a stub by
correcting its top-level help description. The branch
*does* have a real subcommand (`regenerate`), but its
help text claims "Inspect and manage canonical project
checkouts" which led an inspector to look for missing
`list`/`show` subcommands.

## Bead(s)

- `bd-2886bb` — P3 bug, test-user-hel filed.

## Before state

- `caco checkout` description: "Inspect and manage
  canonical project checkouts (bd-5f6b62)."
- Real surface is just `caco checkout regenerate
  --project X`.
- Read-only inspection lives at `caco ls --kind checkout`.
- The mismatch made the branch look like a stub: no
  list/show subcommand despite the "Inspect" promise,
  and the bd-5f6b62 reference is a CLI internal id, not
  a queryable bead (test-user reasonably tried
  `caco bd show --bead-id bd-5f6b62` and got "not
  found").

## After state

- New description: "Manage canonical project checkouts
  (currently: regenerate only). For read-only inspection
  use `caco ls --kind checkout`."
- Removes the stale "Inspect" promise and points users
  at the right surface.
- bd-5f6b62 internal-id reference is now in a code
  comment, not in user-facing help text.

## Diff summary

- 1 file touched, +5 / −2:
  - `crates/caco-cli/src/lib.rs`: CHECKOUT_SUBCOMMANDS
    branch description rewrite + clarifying comment.

## Verification

- `cargo build -p caco-cli`: clean.

## Operator-takeaway

Family with bd-2b10dd / bd-30fbfb (CLI honesty pass) —
help text should describe what's actually there, not
aspirational scope. If/when read-only checkout
inspection lands as `caco checkout list/show`, the
description can drop the "currently: regenerate only"
qualifier.
