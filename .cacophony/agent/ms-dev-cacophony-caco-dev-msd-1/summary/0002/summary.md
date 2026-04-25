# Session summary — AKS steady-state verification

## Goal

Close the loop on the production AKS self-contained multi-role rollout after the node/RBAC blocker was resolved, without taking additional mutating production actions. The session focused on read-only verification, documenting the steady-state evidence, and identifying any remaining validation-surface friction.

## Bead(s)

- `bd-decf57` — Roll out self-contained AKS multi-role topology to production
- Follow-up draft: `bd-940a2a` — Split AKS status/dry-run dev shell from Terraform dependency closure

## Before state

- Failing tests: none in the repository; production status in the bead description said all role pods were Pending because the only AKS node had become NotReady and ms-mac lacked AKS RBAC for recovery.
- Relevant metrics: prior `bd-a6461f` blocker had just closed, but `bd-decf57` still needed verification that CA, relay, master, and three worker roles reached steady state.
- Context: production had already been rolled to the self-contained role topology; the remaining work was safe read-only confirmation and documentation, not another blind Helm upgrade.

## After state

- Failing tests: none introduced. The hermetic `nix develop .#aca --command just aks-self-dry-run` validation attempt timed out after 900s while building Terraform dependencies before reaching kubectl/helm.
- Relevant metrics: Kubernetes reported `caco-aks` `3/3`, `caco-aks-ca` `1/1`, `caco-aks-master` `1/1`, `caco-aks-relay` `1/1`; all six pods were `Running`, container-ready, and at zero restarts; a socket probe from master connected to CA on port `8443`.
- Context: relay, master, and worker pods showed supervisor-managed daemons reachable when checked with explicit `CACO_NODE`; the CA role correctly runs `caco cert serve` rather than `caco daemon`.

## Diff summary

- Commits: `be8c6aade` (documentation), plus the recorded-summary commit containing this file.
- Files touched: `deploy/aks/PRODUCTION-ROLLOUT.md`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-1/summary/0002/summary.md`.
- Tests: +0 / -0 / flipped 0.
- Behavioural delta: no runtime code changed; the production rollout notes now capture the final steady-state evidence and the remaining non-blocking validation-shell and CA-log warnings.

## Operator-takeaway

The AKS self-contained topology is now visibly up at the Kubernetes pod/statefulset level after node recovery: CA, relay, master, and all three workers are running with zero restarts. The remaining improvement is operator ergonomics: quick AKS status/dry-run checks need a lighter hermetic shell than the Terraform-heavy `.#aca` environment.
