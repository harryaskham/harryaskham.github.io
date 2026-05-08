# bd-c7864d AKS caco-aks-1 daemon recovery

Timestamp: 2026-05-08 UTC

## Summary

Hydrated the AKS rollout ledger, checked inbox, and inspected AKS-labelled rollout/blocker beads without generic auto-claim. The main board initially had no open/in-progress AKS, Kubernetes, Helm, or Terranix beads and no existing caco-aks assignment. A fresh private production AKS status check found one actionable degradation: `caco-aks-1` was peer-unreachable while the rest of the non-CA topology was reachable/config-matched and `aks-beads` remained fresh.

Filed and claimed `bd-c7864d` for the scoped production AKS issue. Targeted read-only inspection with `CACO_AKS_POD=caco-aks-1 caco @cluster:caco-aks ...` showed the pod had only PID 1 `caco supervisor` alive, `daemon.pid` pointed at that supervisor PID, no daemon listener was present, and the supervisor was repeating the `bd-a06a10` bounded-backpressure message instead of restarting the daemon. A narrow first-party in-pod lifecycle restart recovered `caco-aks-1`; follow-up status showed it reachable/config-matched again and production AKS `ok: true` with only the expected CA cert-serve-only daemon-unreachable exception.

## Changes

- Updated `deploy/aks/PRODUCTION-ROLLOUT.md` with the 2026-05-08 recovery receipt for `bd-c7864d`.
- Filed follow-up `bd-6ab18f` for the product/lifecycle bug: container PID1 supervisor mode should not remain indefinitely degraded when `daemon.pid` points at the supervisor and no daemon listener exists.

## Validation / evidence

- `caco @cluster:caco-aks status --json true` before recovery: `caco-aks-1` actionable unreachable, other non-CA peers reachable/config-matched, CA expected cert-serve-only.
- `CACO_AKS_POD=caco-aks-1 caco @cluster:caco-aks status --json true` / `ps --json true`: local daemon unhealthy, only supervisor owner present.
- `CACO_AKS_POD=caco-aks-1 caco @cluster:caco-aks restart --skip-update --json true`: narrow first-party recovery for only the affected pod; local exec ended with 137 as the target process restarted.
- Follow-up `caco @cluster:caco-aks status --json true`: production `ok: true`, `caco-aks-1` reachable/config-matched, launcher drift clear.
- `caco @cluster:caco-aks bd status --json true`: `aks-beads` fresh, `ahead: 0`, `behind: 0`.

## Remaining work

- Land this ledger update and close `bd-c7864d` after verifying the landed commit.
- `bd-6ab18f` remains open for the lifecycle self-heal fix.
