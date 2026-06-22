# Technical-writer summary — cacheless docs FINAL settled framing + deploy/aks stale-claim fix

## Goal

Land the settled (final) cacheless-Nix guidance both controllers consolidated, and
fold in config-helper's stale-doc finding. Final per-node picture: ms-dev + ms-dev-2
fully clean (plain nix, no override); ms-mac host clean (Determinate) but a stale
collective eval-cache pins dead redhill, so ms-mac ONE-OFF nix commands keep the
cache.nixos.org override (works) while ms-mac QUEUED cargo (`caco test run`) must be
AVOIDED (the override can't fix it — the daemon realizes the dev-shell with the
eval-cached redhill before the agent command) until the daemon eval-cache fix.

## Bead(s)

- No implementation bead — operator/controller-directed documentation accuracy
  (technical-writer maintenance). config-helper routed the deploy/aks stale-doc
  finding (cross-lane diligence handoff). Settled per ctrl's final consolidated
  guidance — no further cache-form changes expected.

## Before state

- AGENTS.md (L191 + Dev Workflow), README, deploy/aca/README said "ms-mac queued
  cargo keeps the override" (imprecise — the override does not fix queued cargo on
  ms-mac). deploy/aks/PRODUCTION-ROLLOUT.md:2203 still claimed the ACA Attic cache
  "remains a separate, actively-used substituter and is NOT torn down" (stale/wrong).

## After state

- All four cacheless surfaces now carry the settled per-node framing with the
  one-off-vs-queued-cargo split for ms-mac, and explicitly say to AVOID queued cargo
  on ms-mac until the daemon NIX_CONFIG/eval-cache fix.
- deploy/aks/PRODUCTION-ROLLOUT.md corrected: the ACA Attic cache has also been
  retired; durable host config uses the full multi-cache; the separate
  nixpkgs-terraform.cachix.org extra-substituter is noted as a different, unaffected
  cache (per config-helper, flake.nix left untouched).
- deploy/aca/README.md:20 was already corrected earlier (config-helper's :20 was a
  stale snapshot).

## Diff summary

- Files touched: AGENTS.md (×2), README.md, deploy/aca/README.md,
  deploy/aks/PRODUCTION-ROLLOUT.md. No docs/ HTML siblings. No AUTOGEN churn.
- Tests: n/a (docs-only). Behavioural delta: documentation only.

## Operator-takeaway

Cacheless docs now match the settled picture (ms-mac one-off override OK; avoid
ms-mac queued cargo until the daemon fix) and the second stale doc (deploy/aks) is
corrected. Cacheless arc closed at the final accurate state. Revert ms-mac's
queued-cargo caveat once its daemon eval-cache fix lands.
