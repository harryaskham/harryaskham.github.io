# Technical-writer summary — ACA-retirement scope clarification (Attic cache)

## Goal

Address an operator clarification (from ms-dev-2) that the codebase notes about
"retired ACA" refer to the old `caco-aca-*` compute nodes, NOT the operator's
Attic Nix binary cache hosted on ACA, which is still actively used. Prevent a
reader from over-reading the "retired ACA" framing (and the bd-7c031d
storage-account decommission) to sweep up that unrelated, active infrastructure.

## Bead(s)

- No implementation bead — operator-requested documentation accuracy fix
  (technical-writer maintenance). References bd-7c031d (the ACA storage-account
  decommission) for context.

## Before state

- `deploy/aca/README.md` framed the whole ACA path as retired without scoping
  what "retired" covers.
- The published repo docs (README/SPEC/AGENTS/docs/) do not mention the Attic
  cache at all; the only Nix cache referenced in `deploy/aca/flake.nix` /
  `deploy/aks/flake.nix` is the `nixpkgs-terraform.cachix.org` substituter.
- Risk: an agent doing "ACA decommission" could assume the Attic cache is also
  retired.

## After state

- `deploy/aca/README.md` gains a third header blockquote ("Scope of 'retired'")
  clarifying that "retired ACA" means the compute-node worker-apps deploy path
  (residual `caco-aca-*` apps + Terraform-state storage accounts under bd-7c031d)
  only, and explicitly that an operator-managed Nix binary cache (Attic) hosted
  on ACA remains in active use and must not be decommissioned with the
  worker-apps path.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `deploy/aca/README.md` (one added clarifying blockquote; no
  `docs/` HTML sibling — this file is not part of the Pages site).
- Tests: n/a (docs-only).
- Behavioural delta: documentation only.

## Operator-takeaway

"Retired ACA" in this repo is scoped to the ACA compute-node worker-apps deploy
path; unrelated infrastructure hosted on ACA (notably the operator's active Attic
Nix binary cache) is out of scope and must be preserved. If the operator wants the
Attic cache documented as a first-class active dependency (URL/keys), that is a
separate docs page pending the config details.
