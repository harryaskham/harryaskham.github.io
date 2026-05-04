# bd-578d86 — rollout to e5ff14104 and pi-caco-commands guard

The first full AKS image rollout after the `pi-sudo-runner` guard completed:

- ACR remote build pushed `harryaskhamcacoacr.azurecr.io/cacophony:e5ff141045aa`
  and `latest` with digest
  `sha256:d2e561693f674ff1987d780e06f6b87e52228d22e429c5a2e1f093f6b153129c`.
- Helm upgraded `cacophony-aks` to revision 57.
- All Cacophony StatefulSets moved to image tag `e5ff141045aa`.
- Production status returned `ok`, version `1.2.659`, with all six pods Running.

After the rollout, `origin/main` advanced to `81f6dbf05933` and introduced the
new Pi-only `pi-caco-commands` profile. Since the live image predates that
profile and the guarded rollout path pushes ConfigMap before the next image
build, I added `pi-caco-commands` to the self-contained AKS unsupported-profile
filter and validator.

Validation used only first-party AKS surfaces, no raw local cargo:

- `./deploy/aks/validate-self-contained-config.sh`
- `./deploy/aks/validate-operator-surfaces.sh`
- `CACO_AKS_CONTEXT=caco-aks CACO_AKS_NAMESPACE=cacophony just aks-self-dry-run`

The repaired ConfigMap was pushed with `just aks-push-config`; all six pods
remained/recovered Running on `1.2.659`, and relay/master/workers reported config
match. Next step: land this guard and resume `just aks-deploy-main` to roll the
newer `81f6dbf05933` image.
