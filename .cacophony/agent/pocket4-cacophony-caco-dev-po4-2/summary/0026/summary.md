# Session summary — AKS nodepool helpers now classify Azure RBAC blockers clearly

## Goal

Improve the repo-owned hermetic AKS recovery path so operators and agents can
quickly distinguish a broken local Azure CLI from a genuine Azure RBAC blocker.
The live incident on `bd-a6461f` showed that the tooling path itself worked,
but ms-mac's principal lacked rights on both AKS cluster scope and the managed
node resource group. The goal was to make that failure mode explicit and
actionable instead of leaving operators with opaque `AuthorizationFailed` JSON.

## Bead(s)

- `bd-a6461f` — [AKS] Production node NotReady and ms-mac Azure principal cannot inspect/recover nodepool

## Before state

- `just aks-nodepool-show` and `just aks-nodepool-scale` were thin wrappers that
  directly shelled into `nix develop .#aca --command az ...`.
- When Azure returned `AuthorizationFailed`, operators only saw raw Azure CLI
  stderr. The helpers did not explain whether the problem was:
  - broken ambient Azure tooling,
  - missing AKS cluster / agentPool permissions,
  - or missing `Microsoft.Resources/subscriptions/resourceGroups/read` on the
    managed node resource group.
- Fresh incident evidence added to `bd-a6461f` showed the ms-mac principal was
  blocked on both AKS RP scope (`managedClusters/read`, `agentPools/read`) and
  the managed-node RG scope (`MC_harryaskham-sandbox_caco-aks_eastus`).

## After state

- Added `scripts/aks-nodepool-helper.sh` as a repo-owned wrapper for
  nodepool inspection/recovery.
- `just aks-nodepool-show` and `just aks-nodepool-scale` now route through that
  helper instead of calling `az` directly.
- On Azure `AuthorizationFailed`, the helper now prints a clear diagnosis that:
  - the hermetic ACA nix shell is working,
  - the current principal lacks required Azure RBAC,
  - cluster-scope `managedClusters/*` / `agentPools/*` permissions are missing
    when Azure says so,
  - managed-node RG `resourceGroups/read` is missing when Azure says so,
  - and the managed node resource group name is surfaced when present.
- README, AGENTS, and `deploy/aca/README.md` now document that the nodepool
  helpers classify Azure RBAC blockers explicitly rather than emitting only raw
  Azure JSON.
- Live operational recovery is now in msm-2's lane after operator-authorized
  AKS management-plane access was restored there; this landed slice is the
  durable repo-side diagnostic improvement around that incident.

## Diff summary

- Files touched:
  - `scripts/aks-nodepool-helper.sh`
  - `justfile`
  - `README.md`
  - `AGENTS.md`
  - `deploy/aca/README.md`
- Validation:
  - `bash -n scripts/aks-nodepool-helper.sh`
  - mocked `AuthorizationFailed` path via temporary fake `nix` binary to verify
    the helper emits AKS scope + managed-node-RG diagnostics
  - `git diff --check`
- Behavioural delta:
  - hermetic AKS nodepool helpers now fail with operator-actionable RBAC
    guidance instead of opaque Azure CLI stderr

## Operator-takeaway

This change does not recover the AKS node by itself; it makes the repo-owned
recovery path tell the truth faster. When Azure blocks nodepool inspection or
recovery, the helper now says that the hermetic tooling is fine and the current
principal is missing specific AKS / managed-node-RG rights, which avoids wasted
retries and points operators straight at the real Azure RBAC fix.
