# Session summary — bd-376ea1 Wear OS SSH-tunnel, Slice 1 (pure config foundation)

## Goal
Begin bd-376ea1 (Wear OS: SSH port-forward daemon connection — SSH material on
watch, no Tailnet), the Wear OS parity of the phone SSH-tunnel arc I built
(bd-656160 / bd-503a29) and watchOS bd-d36f5b. Oracle: complexity 5/5, decompose,
profile caco-android. Decomposed pure-foundations-first, watch-module-local.

## What landed this reintegration (Slice 1 — pure foundation)
New, purely additive (no edits to existing wearable sources), so zero collision
with md2-0's landed Wear surfaces:
- `companion/android/wearable/src/main/java/com/cacophony/companion/wear/connection/WatchSshTunnelConfig.kt`
  — watch-local SSH-tunnel config contract mirroring the phone's
  `SshTunnelConfig`/`SshTunnelForwardSpec` but watch-scoped:
  - `WatchSshTunnelConfig` data class (host/port=22/username/keyAlias/remoteHost=
    localhost/remotePort/localPort/enabled). Ports default to
    `WatchConnectionConfig.DEFAULT_PORT` (never a literal).
  - API-forward ONLY (no ttyd/web multi-forward — the watch is constrained).
  - Key referenced by Keystore `keyAlias` (not a filesystem keyPath); actual key
    material arrives via the Wearable Data Layer in a later slice.
  - `watchSshTunnelForwardSpec`, `watchSshTunnelConfigComplete`,
    `watchTunneledDaemonBaseUrl`, `watchSshTunnelDaemonEndpoint`
    ({127.0.0.1, localPort, mtlsEnabled=false}), `WatchSshTunnelState` enum,
    `watchSshTunnelReconnectBackoffMs` (exponential, capped, overflow-safe).
  - Pure: no SSH library, sockets, threads, or SharedPreferences.
- `companion/android/wearable/src/test/java/com/cacophony/companion/wear/WatchSshTunnelConfigTest.kt`
  — 6 tests: defaults, loopback base URL, completeness gate, forward-spec mapping,
  loopback+never-mTLS endpoint, reconnect backoff curve.

## Design (confirmed with md2-0, connection-mode owner)
Both md2-0's Wear surfaces (terminal `buildWatchAgentPtyWebSocketUrl`, assistant
tile `cfg.mode==DirectDaemon`) read `WatchConnectionConfig` as the SINGLE endpoint
source. So the later connect-seam slice will, on tunnel-up, set
`WatchConnectionConfig.host=127.0.0.1 / port=localPort / mtlsEnabled=false` and
KEEP the bearer token — driving `WatchConnectionConfig` as the single source so
all surfaces route through the tunnel transparently, no parallel connection path,
no change on md2-0's side (the phone's Option-A pattern). `watchSshTunnelDaemonEndpoint`
already returns exactly that shape.

## Next slices
(2) Wearable-Data-Layer key transfer phone->watch Keystore; (3) sshj forwarder
(check/add wearable SSH dep); (4) WatchConnectionManager connect-seam; (5)
WatchSettings UI (full :wearable:testDebugUnitTest); (6) foreground/runtime-limit
service.

## Validation
Queued `:wearable:testDebugUnitTest --tests WatchSshTunnelConfigTest` (tj-24d77efc)
PASSED (exit 0), host-safe via the daemon test queue during host load. No forbidden
port literals (11100/7682/11180) in either file (verified). Reint gate is currently
echo-disabled, so this real queued gradle run is the validation of record.

## Diff
See the reintegration receipt for the final landed squash SHA.
