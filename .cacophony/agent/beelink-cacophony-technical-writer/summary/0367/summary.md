# Technical-writer summary — correct ACA Attic-cache note to "retired" (operator reversal)

## Goal

Correct the now-inaccurate ACA/Attic documentation. An earlier operator message
said the Attic Nix binary cache on ACA remained active, so a scope note was added
to `deploy/aca/README.md` stating it "remains in active use; do not decommission."
A LATER operator decision (2026-06-22) reversed this: the ACA Attic cache is now
ACTUALLY RETIRED (ACA settings changes made it untenable). Correct the docs to
match, document the current workaround, and note the future plan.

## Bead(s)

- No implementation bead — operator-directed documentation accuracy correction
  (technical-writer maintenance). Related: a peer draft bd-8139f7 tracks the same
  reversal; flagged by caco-tui-msd-0.

## Before state

- `deploy/aca/README.md` scope note (added after the first operator message)
  incorrectly stated the operator-managed Attic Nix binary cache hosted on ACA
  "remains in active use" and "do not decommission it."

## After state

- `deploy/aca/README.md` now states the Attic cache has ALSO been retired (a later
  operator decision; ACA settings changes made it untenable), explicitly noting the
  earlier "remains active" note was superseded. It documents that there is
  currently no shared Attic / Nix binary cache, the stopgap of running Nix with an
  empty substituter list (`nix --option substituters '' <args…>`) to avoid stalling
  on the dead cache, and the planned local `atticd` on ms-dev / ms-dev-2 / ms-dev-3.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `deploy/aca/README.md` (the scope-note blockquote, corrected and
  split into the worker-apps scope plus the Attic-retired status). No `docs/` HTML
  sibling — this file is not part of the Pages site.
- Tests: n/a (docs-only).
- Behavioural delta: documentation only.

## Operator-takeaway

The ACA Attic Nix binary cache is retired as of 2026-06-22 (no active cache now);
the docs no longer claim it is active. Until a local `atticd` lands on
ms-dev/-2/-3, run Nix with `--option substituters ''` to bypass the dead cache.
If the operator wants this workaround surfaced more prominently than the ACA
archaeology README (e.g. the root README build section or a Nix doc), that is a
quick follow-up.
