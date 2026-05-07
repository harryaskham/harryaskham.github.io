# bd-c94a68 AKS toolchain preflight timeout guard

## Problem

After bd-aa5a19 landed, caco-aks resumed bd-1d1f32. Fresh AKS health was OK, but the pinned `just aks-deploy-check` hung in the AKS host toolchain preflight before any production mutation. Narrowing showed the local Nix/macOS toolchain could hang while importing Python/PyYAML (`python3 import yaml`), and a broken preflight could therefore wedge rollout instead of failing bounded with diagnostics.

## Change

Updated `scripts/aks-toolchain-check.sh`:

- `run_check` now wraps subprocess checks in `timeout` when available.
- Added `CACO_AKS_TOOLCHAIN_CHECK_TIMEOUT_SECS`, default `60`, to control the per-command budget.
- Timeout exits are reported as `command timed out after Ns`.
- Replaced the code-signature classifier's `grep` pipeline with shell pattern matching so classification itself does not depend on a potentially broken Nix `grep` process while diagnosing host-toolchain failures.

Updated `deploy/aks/README.md` to document the new timeout override.

## Validation

- `bash -n scripts/aks-toolchain-check.sh`
- `./scripts/aks-toolchain-check.sh --help` contains `CACO_AKS_TOOLCHAIN_CHECK_TIMEOUT_SECS`
- `timeout 80s bash -c 'CACO_AKS_TOOLCHAIN_CHECK_TIMEOUT_SECS=3 ./scripts/aks-toolchain-check.sh acr ...'` exits `1` and reports `command timed out after 3s` diagnostics instead of hanging.
- `git diff --check`

Full AKS validation (`./deploy/aks/validate-operator-surfaces.sh`) is currently blocked on this host because the same Python/PyYAML toolchain problem hangs during self-contained config validation; this bead specifically bounds the preflight hang so production rollout fails safely before mutation.

## Next

After landing, close bd-c94a68 and return to bd-1d1f32. The production rollout remains blocked until the ms-mac AKS operator Python/PyYAML toolchain is repaired or the rollout is routed to a working AKS operator host.
