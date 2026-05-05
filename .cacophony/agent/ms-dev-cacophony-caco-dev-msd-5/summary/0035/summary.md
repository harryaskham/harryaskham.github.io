# bd-4e3d0c — AKS host toolchain preflight guard

Implemented a first-party AKS operator host preflight so broken macOS/Nix/Azure tooling fails before production AKS ConfigMap/Helm mutation or ACR source upload.

## Changes

- Added `scripts/aks-toolchain-check.sh` with modes:
  - `read-only`: bash, `mktemp`, Python+PyYAML, kubectl, helm, and optional kube API/context probe.
  - `deploy`: read-only checks plus git and Azure CLI importability.
  - `acr`: deploy checks plus `az account show`.
- Detects macOS/Nix code-signing failures such as `dyld`, `Library not loaded`, `code signature ... not valid`, and `library load mig callout failed`, and prints an explicit route/repair message instead of letting rollout continue.
- Wired `just aks-toolchain-check` into the justfile.
- Guarded AKS mutation paths:
  - `just aks-deploy-main` runs deploy preflight before fetch/decision and ACR preflight before image-build-required cluster mutations.
  - `just aks-push-config` runs read-only preflight before ConfigMap replacement / StatefulSet restart.
  - `just deploy-remotes` runs ACR preflight before archive/upload/build/Helm rollout.
- Updated AKS static/operator validations to assert the helper, just recipe, docs, and syntax.
- Updated `AGENTS.md`, `README.md`, `deploy/aks/README.md`, `docs/aks.html`, and `deploy/aks/PRODUCTION-ROLLOUT.md` with operator guidance and blocker evidence.

## Validation

- `bash -n scripts/aks-toolchain-check.sh`
- `bash -n deploy/aks/validate.sh`
- `bash -n deploy/aks/validate-operator-surfaces.sh`
- `git diff --check -- AGENTS.md README.md deploy/aks/PRODUCTION-ROLLOUT.md deploy/aks/README.md docs/aks.html justfile scripts/aks-toolchain-check.sh`
- `nix develop .#aks-lite --command bash -lc 'CACO_AKS_CHECK_KUBE=0 ./scripts/aks-toolchain-check.sh read-only'` passed with the expected kube-probe skipped warning.
- `./deploy/aks/validate-operator-surfaces.sh` passed: 34 passed, 0 warnings, 0 failed.
- `./deploy/aks/validate.sh` passed: 97 passed, 0 warnings, 0 failed.
- `./docs/validate-pages.sh` passed: 3313 passed, 0 warnings, 0 failed.

## Routing evidence

- `ms-mac` still demonstrates the original code-signing failure intermittently via remote `caco @ms-mac` command startup (`libgmp.10.dylib` rejected by macOS code-signing).
- `helsinki` has Nix `.#aks` / `.#aks-lite` available, but current evidence shows no kube current-context and no Azure login (`az account show`: please run `az login`), so it is not yet a ready deploy host without operator credential/context setup.
