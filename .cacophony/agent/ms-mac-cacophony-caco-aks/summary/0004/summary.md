# Session summary — AKS relay rollout size convergence

## Goal

Finish the last AKS convergence gap found by the documented dry-run helper: make the live relay StatefulSet template and future remote-build rollouts agree on the expanded relay PVC size.

## Bead(s)

- `bd-2d1ffe` — Restore AKS cluster health and ergonomic local access

## Before state

- Failing tests: `just aks-self-dry-run-lite` failed against the live cluster because the relay StatefulSet volumeClaimTemplate still rendered as `2Gi` while the live PVC had been expanded to `20Gi`.
- Relevant metrics: all AKS pods were Running on image `6cc6561f29f5`, but Helm `--reuse-values` preserved the old relay `2Gi` template.
- Context: Kubernetes forbids changing StatefulSet volumeClaimTemplates in place, so the live object needed a controlled orphan/recreate rather than another normal apply.

## After state

- Failing tests: none in targeted AKS validation.
- Relevant metrics: relay StatefulSet template now reports `20Gi`; `just aks-self-dry-run-lite` returns `AKS self-contained server dry-run OK`; AKS pods remain Running.
- Context: future `just deploy-remotes` runs now pass `roles.items.relay.persistence.size` explicitly from `CACO_AKS_RELAY_PERSISTENCE_SIZE` (default `20Gi`) so Helm `--reuse-values` cannot revert the relay template to the old size.

## Diff summary

- Commits: `c582e9baf`
- Files touched: `justfile`, `README.md`, `AGENTS.md`, `deploy/aks/README.md`, `deploy/aks/PRODUCTION-ROLLOUT.md`
- Tests: `./deploy/aks/validate-self-contained-config.sh`, `./deploy/aks/validate.sh`, `./deploy/helm/validate.sh`, and live `just aks-self-dry-run-lite`.
- Behavioural delta: remote AKS image rollouts explicitly preserve the relay PVC size, and docs list `CACO_AKS_RELAY_PERSISTENCE_SIZE` as the override.

## Operator-takeaway

The recovered AKS cluster now converges through the same documented dry-run/Helm surfaces used for normal operations; relay storage is no longer a hidden manual patch that future image rollouts could accidentally undo.
