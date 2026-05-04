# bd-143ca0 — AKS pi-inbox profile drift guard

## Scope

Production AKS partially degraded after recent pod restarts because the live image (`1.2.634`, tag `251aa950c167`) predates the newly introduced repo-owned `pi-inbox` profile while the rendered self-contained AKS ConfigMap included `pi-inbox` in Pi runtime profiles.

## Evidence

- `kubectl --context caco-aks -n cacophony get pods --no-headers` showed 4/6 Cacophony pods Running.
- `caco-aks-ca-0` and `caco-aks-relay-0` were `CrashLoopBackOff`.
- CA/relay logs failed config validation with: `project 'cacophony' agent_defaults.profile 'pi-inbox' does not match any configured profile`.
- `caco @cluster:caco-aks bd status --json true` still reported `aks-beads` fresh with `ahead: 0` / `behind: 0`.

## Changes

- Updated `deploy/aks/render-config.sh` so self-contained AKS rendering filters `pi-inbox` along with existing unsupported Pi overlay mixins.
- Applied the same unsupported-profile filtering to `interactive_defaults.pi.profile`, not only project `agent_defaults.profile`.
- Extended `deploy/aks/validate-self-contained-config.sh` to assert unsupported Pi-only profiles are absent from both rendered project agent defaults and Pi interactive defaults.
- Recorded the incident and validation receipts in `deploy/aks/PRODUCTION-ROLLOUT.md`.

## Validation

- `./deploy/aks/validate-self-contained-config.sh` — passed.
- Render smoke confirmed `pi-inbox` and `pi-image-guard` are absent from the self-contained render; rendered config size was 347205 bytes.
- `git diff --check` — passed.
- `./deploy/aks/validate-operator-surfaces.sh` — passed, 30 passed / 0 warnings / 0 failed.
- `CACO_AKS_CONTEXT=caco-aks CACO_AKS_NAMESPACE=cacophony just aks-self-dry-run` — passed; only existing kubectl last-applied annotation warnings.

## Production note

No live production ConfigMap push or StatefulSet restart was performed in this implementation pass. Applying the repaired config remains a production mutation and should go through the standard guarded AKS config rollout path unless the operator treats bd-143ca0 as an emergency repair.
