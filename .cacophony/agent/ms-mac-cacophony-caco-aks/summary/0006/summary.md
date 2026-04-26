# Session summary — AKS revision 34 recovery validation

## Goal

Record the live AKS recovery and validation pass after the production rollout temporarily lost schedulable nodes during a fresh image upgrade, while preserving the operator-facing handoff ledger.

## Bead(s)

- `bd-2d1ffe` — Restore AKS cluster health and ergonomic local access

## Before state

- Failing tests: live Helm revision 33 had failed with `context deadline exceeded`; all Cacophony pods were Pending after all three AKS nodes became `NotReady` with unreachable taints.
- Relevant metrics: the attempted image was `harryaskhamcacoacr.azurecr.io/cacophony:5a8ddc42819e`; the system nodepool was at 3 nodes and all were unreachable.
- Context: the correct recovery path was AKS nodepool capacity replacement, not Kubernetes-only taint manipulation or PVC deletion.

## After state

- Failing tests: none in the targeted AKS validation pass.
- Relevant metrics: the system nodepool was scaled to 6 fresh Ready nodes; Helm release `cacophony-aks` converged to revision 34 `deployed`; all CA, relay, master, and worker pods are `Running`; `caco @cluster:caco-aks bd sync` succeeds and bead status reports sync `fresh`.
- Context: live image tag is `5a8ddc42819e`; the in-pod canonical repo checkout was observed at current main `27b04133181c`, whose post-image delta was docs/profile/Android plus Cargo metadata rather than daemon/AKS runtime Rust code.

## Diff summary

- Commits: `6dc10ae55`
- Files touched: `deploy/aks/PRODUCTION-ROLLOUT.md`
- Tests: live `kubectl get nodes`, `kubectl get sts,pods,pvc`, `helm status`, `caco @cluster:caco-aks version --json`, `caco @cluster:caco-aks bd sync`, `caco @cluster:caco-aks bd status --json true`, `caco @cluster:caco-aks status --json true`, `just aks-self-dry-run-lite`, and real-TTY `caco @cluster:caco-aks tui` smoke through `tmux-cli`.
- Behavioural delta: no runtime code changed in this commit; the durable rollout ledger now records the nodepool recovery, revision 34 convergence, beads freshness, and private `@cluster` TUI validation.

## Operator-takeaway

AKS recovered by adding fresh nodepool capacity and is again privately reachable through the ergonomic `caco @cluster:caco-aks ...` path; the only intentional caveat is that the CA role is expected to be daemon-unreachable because it runs `caco cert serve`.
