# bd-578d86 — AKS drift rollout pre-push profile guard

The guarded AKS drift rollout began after validation confirmed production was on
old image `251aa950c167` while target main was `8ae09a49348f`, with both config
and image-relevant drift.

During the ConfigMap pre-push phase, the old live image rejected the newly added
Pi-only `pi-sudo-runner` profile and caused `caco-aks-ca-0` plus
`caco-aks-relay-0` to crash-loop. I updated the self-contained AKS renderer and
validator to filter `pi-sudo-runner` alongside `pi-inbox` and `pi-image-guard`.

Validation used only first-party AKS surfaces, with no raw local cargo:

- `./deploy/aks/validate-self-contained-config.sh`
- `./deploy/aks/validate-operator-surfaces.sh`
- `CACO_AKS_CONTEXT=caco-aks CACO_AKS_NAMESPACE=cacophony just aks-self-dry-run`
- `CACO_AKS_CONTEXT=caco-aks CACO_AKS_NAMESPACE=cacophony just aks-deploy-check`

I pushed the repaired ConfigMap with `just aks-push-config`; all six Cacophony
pods recovered to Running on the existing image, private cluster status returned
`ok`, and AKS beads stayed fresh (`ahead: 0`, `behind: 0`). The full image
rollout still needs to be resumed after this source-side guard lands on main so
`just aks-deploy-main` can safely pre-push config before the ACR build/Helm
rollout.
