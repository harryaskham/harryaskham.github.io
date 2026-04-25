# Session summary — retire stale ACA apps

## Goal

Complete `bd-f093d0`: retire the stale ACA container apps now that AKS is the working production path, and prevent repo-owned workflows from recreating them by default.

## Bead(s)

- `bd-f093d0` — Retire stale ACA container apps after AKS rollout

## Before state

- Failing tests: none; this was an operator-directed cloud cleanup and deploy-flow guardrail.
- Relevant metrics: Azure listed `caco-aca-ca`, `caco-aca-1`, `caco-aca-2`, and `caco-aca-3` in resource group `harryaskham-sandbox` as `Running`.
- Context: Harry clarified ACA is stale/too broad; AKS is working and future compute-node work should focus on dynamic compute nodes rather than ACA-specific implementation.

## After state

- Failing tests: none in validation.
- Relevant metrics: all four named ACA apps were deleted/not found after `az containerapp delete`; `az containerapp list -g harryaskham-sandbox` returned no entries for those names.
- Context: `deploy/aca/deploy.sh --wet-run` now refuses to mutate ACA unless `CACO_ACA_RETIRED_ACK=1`; `just deploy-remotes` skips the ACA leg unless `CACO_DEPLOY_REMOTES_INCLUDE_ACA=1`; repo config no longer defaults bootstrap or static nodes to the deleted `aca-ca` endpoint.

## Diff summary

- Commits: `6ecc35e93`
- Files touched: `.cacophony/config.yaml`, `.cacophony/dynamic_nodes.yaml`, `README.md`, `deploy/aca/README.md`, `deploy/aca/deploy.sh`, `deploy/aca/validate.sh`, `justfile`
- Tests: `az containerapp list` verification, negative `deploy/aca/deploy.sh --wet-run` guard check, `deploy/aca/validate.sh`, `caco config validate --config .cacophony/config.yaml`, `cargo fmt --all -- --check`.
- Behavioural delta: stale ACA apps are removed from Azure, the repo no longer points default bootstrap/relay validation at deleted `aca-ca`, and ACA wet deploys require explicit operator acknowledgement.

## Operator-takeaway

The stale ACA runtime has been taken down and made non-default: normal remote deploys now roll AKS only, while ACA assets remain available only for deliberate archaeology or recovery.
