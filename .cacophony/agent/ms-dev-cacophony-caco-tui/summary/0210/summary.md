# Session summary — clarify ACA-legacy docs exclude the active Attic cache (bd-7dcbdc)

## Goal

Operator correction (Harry, 2026-06-22): the codebase's "ACA is now
legacy/retired" notes are about the old caco-aca compute nodes / ACA Container
Apps deploy path — NOT the Attic Nix binary cache hosted on ACA, which is NOT
retired and is actively in use. Add a clarifying note so an "ACA cleanup" does
not wrongly remove/disable the Attic cache.

## Bead(s)

- `bd-7dcbdc` — Docs: clarify ACA-legacy notes exclude the Attic Nix binary cache
  (still active). [operator-requested; filed + landed this session]

## Before state

- AGENTS.md (~191) and README.md (ACA deploy paragraphs) said "ACA is now
  legacy/reference material" without distinguishing the caco-aca compute nodes /
  ACA deploy path from the Attic Nix binary cache hosted on ACA. The Attic cache
  is not mentioned in the repo docs at all (it is operator-managed; the only nix
  substituters in-repo are the cachix nixpkgs-terraform cache in
  deploy/{aks,aca}/flake.nix), so the legacy notes could be misread as retiring
  the Attic cache too.

## After state

- AGENTS.md + README.md ACA-legacy sections now state the legacy/retired status
  refers ONLY to the caco-aca compute nodes and the ACA Container Apps deploy
  path (`deploy/aca/`), and explicitly that the Attic Nix binary cache hosted on
  ACA is operator-managed, NOT retired, and remains in active use — do not
  remove/disable it during ACA cleanup.

## Diff summary

- Code commit: 1f3ff48823 (final landed squash SHA from the reintegration
  receipt). Summary artefact commit: intentionally omitted.
- Files: `AGENTS.md` (+1 sentence on the ACA-legacy bullet), `README.md` (+1
  sentence on the ACA-legacy paragraph). Docs-only; no code/test change.
- Behavioural delta: none (documentation clarification).
- Validation: prose review for accuracy against the operator's stated fact;
  README + AGENTS kept consistent per the documentation contract.

## Embedded artefacts

- None.

## Operator-takeaway

ACA cleanup must not touch the Attic Nix binary cache. The "ACA legacy/retired"
language only covers the caco-aca compute nodes and the `deploy/aca/` Container
Apps deploy path; the Attic cache on ACA is a separate, operator-managed, active
piece of infrastructure (not referenced in repo nix config). The docs now make
that explicit so no agent retires the cache while cleaning up the legacy ACA
compute path.
