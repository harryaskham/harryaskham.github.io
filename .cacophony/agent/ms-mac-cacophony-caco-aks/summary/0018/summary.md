# bd-578d86 — ACR source upload timeout guard

After successfully rolling production AKS to image `62938949b7a6` / version
`1.2.662`, main advanced again and `just aks-deploy-main` attempted another ACR
remote build. The ConfigMap pre-push succeeded and the cluster stayed healthy,
but `az acr build` failed while uploading the source archive:

```text
AuthenticationFailed: Signed expiry time ... must be after signed start time ...
```

The error happened before remote build execution and did not mutate the live
image. AKS remained on `62938949b7a6`, with all six pods Running.

I updated the repo-owned remote-build helper to pass
`--timeout "${CACO_ACR_BUILD_TIMEOUT_SECS:-7200}"` to `az acr build`, and documented
the override in the AKS docs/README. This should extend the ACR task upload SAS
budget on slow operator-host uploads without changing normal rollout semantics.
