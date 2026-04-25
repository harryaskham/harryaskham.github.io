# Session summary — AKS in-cluster PKI and bootstrap

## Goal

This session made the self-contained AKS cluster use an in-cluster Cacophony PKI authority rather than depending on local `ms-mac`/`helsinki` certificate paths. The CA role now runs the first-party `caco cert serve` foreground service, non-CA roles bootstrap with `CACO_BOOTSTRAP_TOKEN`, and the rendered config can target arbitrary namespaces/headless service names for local Kubernetes validation or future materializers.

## Bead(s)

- `bd-0ddccd` — AKS in-cluster PKI and bootstrap
- parent: `bd-07f7a2` — AKS self-contained Cacophony cluster topology

## Before state

- Failing tests: none known statically, but live local Kubernetes validation had not been attempted.
- Relevant metrics: the chart rendered all roles as `caco up` and applied `caco status` probes to CA-only roles; the topology lacked `bind_host: 0.0.0.0` for static DNS-hosted AKS nodes.
- Context: Docker Desktop Kubernetes became available locally, and a refreshed ACR Linux image was pulled for real container validation under amd64 emulation.

## After state

- Failing tests: no scoped static validation failures. Live local Docker Desktop relay reached PKI issuance but exposed that the already-pulled image lacked the newly committed safe-directory prelude fix, so steady-state daemon validation should be rerun after the next remote image build.
- Relevant metrics: `deploy/helm/validate.sh` passed with 86 checks; `deploy/aks/validate.sh` passed with 65 checks; `CACO_BIN="cargo run -q -p caco --" deploy/aks/validate-self-contained-config.sh` passed; `CACO_CONFIG_PATH= deploy/compose/validate.sh` passed with 38 checks and one expected optional-render warning.
- Context: local Docker Desktop namespace `caco-validate-bd-0ddccd` successfully ran `caco-aks-ca-0` as `caco cert serve`; the CA PVC contained `ca.pem`, `ca.key`, and the CA node cert/key. Enabling `caco-aks-relay-0` produced CA-issued relay cert material, proving non-CA bootstrap issuance.

## Diff summary

- Commits: `34d870013`
- Files touched: `deploy/aks/config/topology.yaml`, `deploy/aks/render-config.sh`, `deploy/aks/validate-self-contained-config.sh`, `deploy/aks/validate.sh`, `deploy/aks/README.md`, `deploy/helm/cacophony/templates/statefulset.yaml`, `deploy/helm/cacophony/templates/headless-service.yaml`, `deploy/helm/cacophony/values.yaml`, `deploy/helm/validate.sh`, `deploy/helm/README.md`, `deploy/compose/container-prelude.sh`, `deploy/compose/validate.sh`
- Tests: Helm render/lint, AKS config validation, compose/prelude safe-directory validation, and local Docker Desktop Kubernetes install/dry-run checks.
- Behavioural delta: the CA role now runs `caco cert serve` with bootstrap port 8443 and probes disabled; non-CA roles keep `caco up`; AKS static nodes bind on `0.0.0.0`; the renderer rewrites AKS DNS for namespace/headless overrides; the container prelude marks the runtime root as a Git safe directory before non-root exec.

## Operator-takeaway

The self-contained AKS cluster now has a portable PKI/bootstrap role contract: durable CA state lives with the CA role, joining nodes obtain AKS-issued mTLS material through the in-cluster bootstrap URL, and the same stable identity/config/secrets/state shape can be reused later for micro-VM or Azure dynamic-compute materializers.
