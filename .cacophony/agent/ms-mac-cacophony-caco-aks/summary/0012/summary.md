# Session summary — AKS revision 48/49 validation record

## Goal

Record the post-recovery AKS production state after the guarded deploy path advanced the cluster through Helm revisions 48 and 49, so future operators can distinguish proven health from transient mainline image drift.

## Bead(s)

- `bd-8d12c7` — Record AKS revision 48/49 rollout validation

## Before state

- Failing tests: none for this docs-only ledger update.
- Relevant metrics: AKS had just rolled to Helm revision 49 on image `harryaskhamcacoacr.azurecr.io/cacophony:cfde24f94a10`; eight nodes were Ready and all Cacophony role pods were Running.
- Context: the production rollout ledger did not yet include the revision 48/49 validation results, ACR run IDs, beads status, repo checkout state, or real-TTY `@cluster` TUI smoke evidence.

## After state

- Failing tests: none; `git diff --check` passed.
- Relevant metrics: `deploy/aks/PRODUCTION-ROLLOUT.md` now records ACR runs `ca25`/`ca26`, Helm revisions 48/49, image tags `e013f1c0c9e2` and `cfde24f94a10`, eight Ready nodes, fresh `aks-beads` status with `ahead: 0` / `behind: 0`, clean in-pod checkout at `90f827d7dcfb`, and successful private `caco @cluster:caco-aks tui` smoke.
- Context: the ledger explicitly notes that newer post-revision-49 image-relevant drift exists, but AKS was otherwise healthy and the run intentionally stopped rather than entering another unbounded rebuild loop.

## Diff summary

- Commits: `5982a32f0`
- Files touched: `deploy/aks/PRODUCTION-ROLLOUT.md`
- Tests: `git diff --check`
- Behavioural delta: no runtime behaviour changed; this is a durable operator-facing rollout record.

## Operator-takeaway

AKS was restored to a working, private, multi-role production deployment and validated after revision 49; future operators should use `just aks-deploy-check` before deciding whether another expensive AKS image rollout is warranted.
