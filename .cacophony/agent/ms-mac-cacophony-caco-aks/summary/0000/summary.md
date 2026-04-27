# AKS operator 101 docs update

## Goal

Publish a beginner-friendly AKS operator guide on the GitHub Pages AKS deployment page so the operator can see how the cluster is named, how to disambiguate it with `@cluster` syntax, how to connect privately, where configuration lives, and how to update or scale it.

## Bead(s)

- `bd-af3853` — Record AKS rollout to current main after reintegration safety hold.

## Before state

- `docs/aks.html` already documented guarded AKS rollout, private `@cluster` access, and nodepool recovery at a technical level.
- It did not have a compact "n00b 101" section listing the concrete production context (`caco-aks`), namespace (`cacophony`), Helm release (`cacophony-aks`), default command pod, config locations, and exact copy/paste health/update/scale commands.
- AKS production had been recovered separately to nodepool count 11 with all Cacophony role pods healthy; this docs change did not need another cluster mutation.

## After state

- Added a `Production cluster 101` section to `docs/aks.html`.
- Documented the production AKS names and the private `caco @cluster:caco-aks ...` workflow.
- Added raw Kubernetes examples through `nix develop .#aks-lite`, heavier Azure/ACR/nodepool guidance through `nix develop .#aks`, repo and in-pod config locations, guarded deploy commands, nodepool scaling commands, and a quick health checklist.
- Preserved the private-by-default contract: access uses kubeconfig plus `kubectl exec`, not public daemon/web/SSE ingress.

## Diff summary

- `docs/aks.html`: inserted a new operator-oriented AKS 101 section before the existing rollout workflow, including commands for `@cluster`, `kubectl`, `helm`, guarded deploy, nodepool scaling, and repo freshness checks.
- `.cacophony/agent/ms-mac-cacophony-caco-aks/summary/0000/summary.md`: recorded this session summary for direct recorded reintegration.

## Validation

- `./docs/validate-pages.sh` — passed (`1781 passed, 0 warnings, 0 failed`).
- `git diff --check` — passed.

## Operator-takeaway

Use `caco @cluster:caco-aks ...` for private Cacophony commands against AKS, `nix develop .#aks-lite` for read-only Kubernetes checks, `nix develop .#aks --command just aks-deploy-check` before any guarded deployment, and `nix develop .#aks --command just aks-nodepool-scale harryaskham-sandbox caco-aks system <count>` for AKS nodepool recovery.
