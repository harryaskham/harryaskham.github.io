# bd-d17ab9: move sidecar status probe off the UI snapshot request path

## Problem
The ms-mac caco-daemon flapped (supervisor restart ~5-10min, ~20min+ sustained)
at MODERATE load (14-28). Root (msd-5's diagnosis): handle_ui_snapshot_inner
(ui_stream.rs) constructed a FRESH LifecycleManager + ran mgr.status() — a ~2s
concurrent TCP-probe of all services — on EVERY snapshot build. Under load the
cumulative sequential build exceeded the 5s UI_SNAPSHOT_TOTAL_DEADLINE -> the
snapshot cache never refreshed -> 13-15min stale snapshots (observed served age
807-889s) -> health degraded -> supervisor restart after the 600s grace -> flap.
A feedback loop amplified it (the probe targets the daemon's own port, slow
because the daemon is busy building snapshots). Distinct from bd-996b25 (load
cap) and bd-fc7dca (degraded-200 mitigation, already in — this is the root).

## Fix (bd-96677e/bd-dc4a4c move-expensive-work-off-request precedent)
- DaemonState gains `sidecar_status_cache: Arc<RwLock<Vec<ServiceStatus>>>`.
- A new off-request "sidecar status refresh loop" (10s cadence, refresh-first,
  embedded-skipped like the other reconcile loops) runs mgr.status() and updates
  the cache OFF the request path.
- handle_ui_snapshot_inner READS the cache instead of probing — removes ~2s/build
  so the build drops well under the 5s deadline, the cache refreshes, the flap
  stops. Sidecar state is bounded-stale by the 10s cadence (identical to the
  bd-96677e 10s agent read-model precedent).
- The map is extracted to a pure `build_service_snapshots()` helper so the
  off-request read path is unit-testable.

## Validation
- cargo check -p caco-daemon: green (4m46s; only pre-existing unrelated warning).
- cargo test -p caco-daemon --lib build_service_snapshots_maps_cached_statuses_bd_d17ab9:
  PASSED (proves the snapshot builds from cached statuses + the ActualState->status
  mapping; empty cache -> empty services, no inline probe fallback).
- Live ms-mac flap-stop confirmation deferred (ms-mac offline = not flapping, per
  the bead + ctrl); the focused test is the agreed real proof.

## Credits
Diagnosis: msd-5. Roadmap (exact spots): aur-1. Implementation + test: aur-4.

## Diff
crates/caco-daemon/src/lib.rs (DaemonState field + 14 construction inits +
background loop + embedded-skip entry), crates/caco-daemon/src/ui_stream.rs
(cache read + build_service_snapshots helper + test). Final landed squash SHA
per the reintegration receipt.

## SPEC areas
- SPEC 15.1 / 20.2 / 20.4: UI aggregate snapshot + service discovery — preserves
  the authoritative services list while moving the expensive probe off the
  request path (consistent with the bd-fc7dca snapshot hardening + the bd-96677e
  off-request read-model pattern).
