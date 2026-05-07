# bd-aa5a19 AKS ACR source-upload retry guard

## Scope

caco-aks hydrated AKS rollout context and inbox, then handled overnight AKS-scoped work only.

- `bd-4d0a5b` was verified as already covered by the landed `bd-57e88a` bootstrap-readiness fix (`1abc6302763e770b216071335f6d811fcd41422e`) and closed via the audited duplicate/superseded path after confirming this persistent is currently running.
- `bd-1d1f32` production rollout was started and parked with evidence: pinned deploy applied the ConfigMap/restarted StatefulSets, then ACR source upload failed before image build with `AuthenticationFailed` / `Signed expiry time` SAS expiry.
- Filed and claimed `bd-aa5a19` to harden the AKS remote-build path before resuming the production rollout.

## Change

Updated the repo-root AKS remote-build recipe:

- `just deploy-remotes` now wraps `az acr build` in a bounded retry loop.
- It retries only when output contains both `AuthenticationFailed` and `Signed expiry time`, matching the Azure Storage SAS-expiry failure observed before build execution.
- Defaults are intentionally conservative:
  - `CACO_ACR_BUILD_UPLOAD_RETRIES=2`
  - `CACO_ACR_BUILD_RETRY_DELAY_SECS=15`
- Non-SAS failures and exhausted retries still exit with the original `az acr build` status.

Docs/ledger updates:

- `deploy/aks/README.md` documents the new retry environment overrides.
- `deploy/aks/PRODUCTION-ROLLOUT.md` records the 2026-05-07 rollout attempt, the live drift evidence, the exact SAS-expiry class, and the retry mitigation.

## Validation

- `just --dry-run deploy-remotes` — rendered the updated recipe successfully.
- `./deploy/aks/validate-operator-surfaces.sh` — passed, 34 checks.
- `./deploy/aks/validate.sh` — passed, 97 checks.
- `git diff --check` — passed.

## Next

After this lands, resume `bd-1d1f32` with a fresh live AKS health check and pinned `CACO_AKS_DEPLOY_REF` deploy-check, then retry the guarded production rollout.
