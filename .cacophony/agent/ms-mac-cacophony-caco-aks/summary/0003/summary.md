# Session summary — AKS fresh image and final health fixes

## Goal

Finish the AKS recovery loop by validating that the cluster is schedulable, running a fresh Cacophony image, serving beads through the private `@cluster` path, and rendering the in-cluster TUI from a real terminal.

## Bead(s)

- `bd-2d1ffe` — Restore AKS cluster health and ergonomic local access
- `bd-9a699d` — Add Terranix AKS operator recovery RBAC
- `bd-f84a2a` — Add `@cluster` shorthand for private AKS local access

## Before state

- Failing tests: no repo validation failures; live AKS had residual role-health issues after the image rollout.
- Relevant metrics: Helm revision 25 had deployed image `harryaskhamcacoacr.azurecr.io/cacophony:6cc6561f29f5`, but relay daemon was not reachable because the original 2Gi relay PVC was full, and CA appeared as a daemon-unreachable peer even though it intentionally runs `caco cert serve`.
- Context: non-interactive `timeout caco @cluster:caco-aks tui` failed because `kubectl exec -it` requires a real TTY, so the TUI path needed tmux-based validation.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: all six role pods are Running on image `6cc6561f29f5`; `caco @cluster:caco-aks version --json` reports `1.2.560`; `caco @cluster:caco-aks bd list` returns the fresh isolated `aks-beads` board; relay PVC is expanded to 20Gi and relay status recovered; CA unreachable is annotated expected/non-actionable.
- Context: a `tmux-cli` real-TTY smoke test of `caco @cluster:caco-aks tui` rendered the in-cluster TUI successfully.

## Diff summary

- Commits: `8f546a8c3`
- Files touched: `deploy/aks/config/topology.yaml`, `deploy/helm/cacophony/values.yaml`, `deploy/aks/PRODUCTION-ROLLOUT.md`
- Tests: `./deploy/aks/validate-self-contained-config.sh`, `./deploy/aks/validate.sh`, `./deploy/helm/validate.sh`, live `caco @cluster:caco-aks status`, live `caco @cluster:caco-aks bd list`, and tmux TUI smoke.
- Behavioural delta: relay roles now default to a 20Gi PVC, and the CA role is documented/configured as an expected non-daemon peer because its health surface is bootstrap HTTPS from `caco cert serve`.

## Operator-takeaway

AKS is now operational via the private `@cluster` path with fresh pods and beads. The main remaining nuance is architectural rather than outage: the self-contained AKS board is intentionally isolated on `aks-beads`, so it is a safe validation board rather than the production beads primary.
