# Session summary — AKS nodepool RBAC blocker

## Goal

Document the production AKS recovery blocker so future agents do not keep retrying unsafe Kubernetes-only fixes or long Azure CLI builds from ms-mac. The node is NotReady/out-of-service, and ms-mac's Azure principal lacks the AKS/nodepool permissions needed to recover it.

## Bead(s)

- `bd-e0ddde` — [AKS] Grant nodepool recovery RBAC for caco-aks
- Related: `bd-a6461f` — [AKS] Production node NotReady and ms-mac Azure principal cannot inspect/recover nodepool

## Before state

- Failing tests: none specific to this documentation change.
- Relevant metrics: `just aks-self-status` showed all caco-aks role pods Pending and PVCs Bound; `kubectl get nodes` showed the single AKS node NotReady; Azure nodepool inspection was blocked by AuthorizationFailed / first-run nix build timeout from ms-mac.
- Context: agents could inspect Kubernetes state but could not perform nodepool/VMSS recovery, and Kubernetes-only pod/toleration actions would not restore capacity.

## After state

- Failing tests: none observed in lightweight validation.
- Relevant metrics: `deploy/aks/PRODUCTION-ROLLOUT.md` now lists `bd-a6461f` / `bd-e0ddde`, required Azure RBAC roles, and the hermetic `just aks-nodepool-show` / `just aks-nodepool-scale` recovery helpers.
- Context: recovery is explicitly documented as requiring an Azure principal with AKS cluster user/contributor permissions on the caco-aks scope.

## Diff summary

- Commits: `7d709e013`
- Files touched: `deploy/aks/PRODUCTION-ROLLOUT.md`
- Tests: docs/grep validation via `deploy/aks/validate.sh` was invoked; this change is documentation-only.
- Behavioural delta: no runtime change; the operator recovery path is clearer and avoids repeated unsafe/no-op attempts.

## Operator-takeaway

The AKS production outage is a nodepool/RBAC problem, not a Helm chart or pod scheduling problem. Give the recovery principal the documented AKS permissions, then use the repo-root hermetic helpers to inspect/scale/repair the nodepool.
