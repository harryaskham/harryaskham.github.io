# Session summary — AKS revision 39 steady state

## Goal

Finish the AKS convergence loop after repeated mainline runtime drift by rolling the production AKS deployment to the latest daemon-lifecycle image that mattered for the cluster, revalidating private `@cluster` access, and recording the final operator handoff state.

## Bead(s)

- `bd-2d1ffe` — Restore AKS cluster health and ergonomic local access

## Before state

- Failing tests: none in the immediate cluster health path, but AKS revision 37/38 image freshness was overtaken by daemon lifecycle changes on `origin/main`.
- Relevant metrics: the cluster had six Ready AKS nodes and all Cacophony role pods Running; image freshness had advanced through revision 38 tag `74e6a4ca9f8a`.
- Context: all builds had to avoid local Rust contention on ms-mac, so the rollout used the repo-owned ACR remote-build path and Helm upgrades only.

## After state

- Failing tests: none in the targeted AKS validation pass.
- Relevant metrics: Helm release `cacophony-aks` reached revision 39 on image `harryaskhamcacoacr.azurecr.io/cacophony:ee9f931cb8eb`; six AKS nodes remained Ready; all CA, master, relay, and worker pods were Running; live `caco @cluster:caco-aks version --json` reported `1.2.564`.
- Context: the in-pod canonical repo checkout was regenerated to current `origin/main` during validation, and AKS beads on `aks-beads` reported sync `fresh`, `ahead: 0`, `behind: 0`.

## Diff summary

- Commits: `74c2f0129`
- Files touched: `deploy/aks/PRODUCTION-ROLLOUT.md`
- Tests: ACR remote build `ca1v`; Helm revision 39; `kubectl rollout status` for all role StatefulSets; `kubectl get nodes/pods`; `caco @cluster:caco-aks version --json`; `caco @cluster:caco-aks status --json true`; `caco @cluster:caco-aks bd sync`; `caco @cluster:caco-aks bd status --json true`; in-pod checkout regeneration; real-TTY `caco @cluster:caco-aks tui`; `just aks-self-dry-run-lite`.
- Behavioural delta: no runtime code changed in this commit; the production rollout ledger now records revision 38/39 image rollout, final private access validation, and the explicit decision not to keep chasing non-AKS/web/version-metadata drift.

## Operator-takeaway

AKS is healthy, private-by-default, and operable from the workstation with `caco @cluster:caco-aks ...`; the remaining expected non-green bit is the CA pod’s daemon-unreachable marker because that role intentionally runs `caco cert serve`.
