# Session summary — AKS deploy decision check

## Goal

Add a no-mutation check mode for the guarded AKS deploy path so operators can see whether AKS would refresh the in-pod checkout, apply Helm-only changes, or start a remote ACR build without accidentally launching a long rollout.

## Bead(s)

- `bd-2d1ffe` — Restore AKS cluster health and ergonomic local access

## Before state

- Failing tests: none in AKS health; the guarded deploy path existed but lacked a safe decision-only command.
- Relevant metrics: AKS remained healthy with eight Ready nodes and all role pods Running; live image was `2ff1b711f710` and current main had moved with CLI/TUI runtime changes.
- Context: after many long ACR builds, operators needed a cheap preflight to avoid surprise rebuilds.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: `just aks-deploy-check` / `CACO_AKS_DEPLOY_DRY_RUN=1 just aks-deploy-main` reports the intended action and exits without checkout refresh, Helm upgrade, or ACR build; targeted AKS validation still reports 82 passed, 0 warnings, 0 failed.
- Context: a live dry-run reported that current main would require an ACR build because CLI/TUI runtime files changed, and made no AKS mutations.

## Diff summary

- Commits: `2f26b0af7`
- Files touched: `justfile`, `README.md`, `deploy/aks/README.md`
- Tests: `just --summary`, `./deploy/aks/validate.sh`, `CACO_AKS_CONTEXT=caco-aks CACO_AKS_DEPLOY_DRY_RUN=1 just aks-deploy-main`, `git diff --check`
- Behavioural delta: adds `just aks-deploy-check` and dry-run behavior in every guarded deploy branch.

## Operator-takeaway

Before a potentially expensive AKS rollout, run `nix develop .#aks --command just aks-deploy-check`; it tells you whether a rebuild would happen without touching production.
