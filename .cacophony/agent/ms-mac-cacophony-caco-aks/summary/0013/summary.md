# Session summary — AKS nodepool recovery to nine nodes

## Goal

Record and land the operator-facing ledger entry for a live AKS nodepool recovery after one system node entered the shutdown/unreachable NotReady state while the Cacophony workload remained healthy.

## Bead(s)

- `bd-3a47de` — Record AKS nodepool recovery to nine nodes

## Before state

- Failing tests: none; this was a production recovery documentation update.
- Relevant metrics: AKS had `7/8` nodes Ready because `aks-system-24353107-vmss00000h` was NotReady with shutdown/out-of-service/unreachable taints; Cacophony pods were still `6/6` Running, private `@cluster` status was `ok: true`, beads were fresh, and the in-pod repo checkout matched main.
- Context: the established recovery policy is to use AKS nodepool operations for shutdown/unreachable nodes instead of Kubernetes-only taint workarounds.

## After state

- Failing tests: none; `git diff --check` passed.
- Relevant metrics: system nodepool was scaled from desired count `8` to `9`; Kubernetes showed `9/9` nodes Ready and `6/6` Cacophony pods Running after replacement nodes joined. Live image stayed `cfde24f94a10`; no Helm or image rollout was performed.
- Context: `deploy/aks/PRODUCTION-ROLLOUT.md` now records the degraded node, recovery commands, replacement nodes, and post-recovery validation.

## Diff summary

- Commits: `8fd01ab1a`
- Files touched: `deploy/aks/PRODUCTION-ROLLOUT.md`
- Tests: `git diff --check`, live AKS node/pod/status/bead/repo checks
- Behavioural delta: no code behaviour changed; the production ledger now captures the nodepool recovery and current steady state.

## Operator-takeaway

When AKS system nodes hit the shutdown/unreachable pattern, the safe recovery remains the hermetic AKS nodepool scale path; this pass restored capacity to nine Ready nodes without touching the Cacophony pods or rolling the image.
