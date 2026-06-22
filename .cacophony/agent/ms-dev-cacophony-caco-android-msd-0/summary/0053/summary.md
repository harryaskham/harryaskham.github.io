# Session summary — bd-376ea1 Wear OS SSH-tunnel, Slice 6 (foreground service) — FINAL

## Goal
Finish bd-376ea1. Slices 1-5b landed (9 slices: the watch opens, routes through,
persists, and now toggles its own Tailnet-free SSH tunnel). This reintegration is the
final Slice 6: a foreground service so the long-running tunnel survives Wear OS
battery/runtime limits — bringing the watch to parity with the phone (bd-656160).

## What landed this reintegration (Slice 6 — FG service)
- `wearable/.../wear/connection/WatchSshTunnelForegroundService.kt` (new):
  - `object WatchSshTunnelSessionHolder` — a singleton holding the active
    `WatchSshTunnelSession`. WatchConnectionManager is a per-consumer class (each
    consumer constructs its own), so the session is published to this holder so the
    foreground service (a separate component) can close it onDestroy. Mirrors the
    phone's SshTunnelManagerHolder.
  - `class WatchSshTunnelForegroundService : Service()` — onCreate ensures the
    notification channel; onStartCommand startForeground(NOTIF_ID=4823, …) +
    START_STICKY; onDestroy closes the held session (safety net); companion
    pure `statusText`, ensureChannel, buildNotification, start/stop. Mirrors the
    phone's SshTunnelForegroundService.
- `WatchConnectionManager`: startSshTunnel publishes the session to the holder and
  starts the FG service; stopSshTunnel clears the holder and stops the service.
- `wearable/.../AndroidManifest.xml`: + FOREGROUND_SERVICE +
  FOREGROUND_SERVICE_SPECIAL_USE permissions + a `<service>` entry with
  `foregroundServiceType="specialUse"` and the required
  `PROPERTY_SPECIAL_USE_FGS_SUBTYPE` property (the wearable's first FG service).
- `WatchSshTunnelForegroundServiceTest`: pure statusText unit test + service/holder/
  manager-wiring/manifest source-pins.

## bd-376ea1 feature-complete (10 slices)
config -> key-transfer payload -> watch listener -> AndroidKeyStore secure storage ->
phone publisher -> on-watch sshj forwarder (TOFU + R8 fix) -> connect-seam -> listener
persistence -> Settings toggle -> foreground service. The Wear OS watch now opens,
routes, persists, toggles, and keep-alives its own Tailnet-free SSH tunnel to the
daemon — parity with phone bd-656160 / watchOS bd-d36f5b.

## Validation
Queued `:wearable:testDebugUnitTest` (tj-7930b947) over the new FG test + connect +
manager source tests PASSED — the service/holder/manager compile and all source-pins
(incl. the manifest FGS entry) are green. Plus a separate `:wearable:assembleRelease`
manifest-merge / R8 build-validation before landing. No forbidden port literals.
On-watch / emulator validation deferred to a remote Android builder (co-validate md2-0).

## Diff
See the reintegration receipt for the final landed squash SHA.
