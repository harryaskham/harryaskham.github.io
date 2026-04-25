# Session summary — AKS production rollout blocked by node outage

## Goal

This session attempted the production rollout from the legacy single-StatefulSet AKS deployment to the self-contained multi-role Cacophony topology.

## Bead(s)

- `bd-decf57` — Roll out self-contained AKS multi-role topology to production
- blocker filed: `bd-535c46` — Repair Azure CLI / AKS node recovery path on ms-mac

## Before state

- Failing tests: none in repo validators.
- Relevant metrics: production server-side dry-run initially failed on existing Namespace/ConfigMap objects; live AKS showed the legacy `cacophony-aks` StatefulSet pending on a NotReady node.
- Context: local Docker remained off-limits; production validation used Kubernetes API and Helm only.

## After state

- Failing tests: none in scoped repo validation.
- Relevant metrics: `deploy/aks/validate-operator-surfaces.sh` passed; `deploy/aks/validate.sh` passed; `just aks-self-dry-run` passed against production after the helper fix.
- Context: Helm release `cacophony-aks` revision 21 is deployed with four multi-role StatefulSets, but every pod is pending because the only AKS node is NotReady with shutdown/out-of-service/unreachable taints.

## Diff summary

- Commits: `8e9ed867f`
- Files touched: `justfile`, `deploy/aks/PRODUCTION-ROLLOUT.md`
- Tests: operator-surface and AKS validators rerun; production server-side dry-run passed before the Helm upgrade.
- Behavioural delta: `aks-self-dry-run` now handles existing production Namespace/ConfigMap objects, and the partial rollout state is documented with the exact blocker and recovery commands.

## Operator-takeaway

The production chart/config rollout was applied, but steady-state validation is blocked by AKS infrastructure: the only node stopped posting status before the rollout. Do not close `bd-decf57` until the nodepool is recovered and the multi-role pods become Ready.
