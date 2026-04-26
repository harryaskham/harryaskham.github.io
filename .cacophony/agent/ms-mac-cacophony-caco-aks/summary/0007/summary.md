# Session summary — AKS revision 37 convergence

## Goal

Finish the AKS production recovery loop by rolling the cluster through the latest daemon-runtime image available during the session, validating private `@cluster` access, confirming fresh repo and beads state, and recording the final rollout facts for operator handoff.

## Bead(s)

- `bd-2d1ffe` — Restore AKS cluster health and ergonomic local access

## Before state

- Failing tests: none in the live AKS health path, but the cluster image was behind fast-moving `origin/main` after revision 34/35 validation.
- Relevant metrics: AKS had six Ready system nodes and Running role pods; live image advanced through `1d7cbf60d5db` and `d9197f59384f` while main continued to move.
- Context: operator requested AKS convergence without local Rust contention on ms-mac, so all image builds were ACR remote builds and validation used lightweight `kubectl`, `helm`, and `@cluster` probes.

## After state

- Failing tests: none in the targeted AKS validation pass.
- Relevant metrics: Helm release `cacophony-aks` is revision 37 `deployed`; all CA, master, relay, and worker StatefulSets are ready on image tag `494d6e0d911e`; six AKS nodes are Ready; in-pod canonical repo checkout is at `cef0c7535554`; AKS beads branch `aks-beads` reports sync `fresh`, `ahead: 0`, `behind: 0`.
- Context: `caco @cluster:caco-aks status --json true`, `bd sync`, `bd status`, and real-TTY `caco @cluster:caco-aks tui` smoke all passed. Main moved again afterward with non-AKS profile/docs/Android/web-static/test/version metadata, so the pass intentionally stopped chasing unrelated drift.

## Diff summary

- Commits: `c746aa63d`
- Files touched: `deploy/aks/PRODUCTION-ROLLOUT.md`
- Tests: remote ACR builds `ca1r`, `ca1s`, and `ca1t`; Helm revisions 35, 36, and 37; `kubectl rollout status` for all role StatefulSets; `kubectl get nodes/pods`; `caco @cluster:caco-aks version --json`; `caco @cluster:caco-aks status --json true`; `caco @cluster:caco-aks bd sync`; `caco @cluster:caco-aks bd status --json true`; in-pod git checkout check; `caco @cluster:caco-aks tui` real-TTY smoke; `just aks-self-dry-run-lite`.
- Behavioural delta: no runtime code changed in this commit; the production rollout ledger now records the final revision 37 convergence and private access validation.

## Operator-takeaway

AKS is healthy and privately operable through `caco @cluster:caco-aks ...`; the only visible non-green peer is the expected CA daemon-unreachable marker because that pod intentionally runs `caco cert serve` rather than the daemon.
