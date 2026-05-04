# bd-143ca0 — live AKS config repair applied

After the source fix for the `pi-inbox` profile drift landed on main at `7d6e19336`, production AKS was still partially degraded: `caco-aks-ca-0` and `caco-aks-relay-0` were crash-looping on the stale rendered ConfigMap while master/workers remained up.

## Live repair

Ran the standard guarded config rollout from this checkout:

```bash
CACO_AKS_CONTEXT=caco-aks CACO_AKS_NAMESPACE=cacophony just aks-push-config
```

The command replaced ConfigMap `cacophony-config`, restarted the Cacophony StatefulSets, and rollout status completed.

## Post-apply validation

- `kubectl --context caco-aks -n cacophony get pods --no-headers` showed 6/6 Cacophony pods Running.
- `caco @cluster:caco-aks status --json true` returned `ok: true`, version `1.2.634`.
- Relay, master, and all three workers were reachable with config match.
- CA remained expected daemon-unreachable because it runs `caco cert serve`; its bootstrap HTTPS surface is the intended health path.
- `caco @cluster:caco-aks bd status --json true` reported `aks-beads` fresh with `ahead: 0` and `behind: 0`.

## Notes

This was a ConfigMap/restart repair only; no container image rollout was performed. The broader production image/config drift item remains tracked by `bd-578d86`.
