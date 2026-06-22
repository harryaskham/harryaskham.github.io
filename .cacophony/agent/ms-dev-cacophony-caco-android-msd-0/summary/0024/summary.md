# Session summary — bd-656160 Slices 1+2a: Android SSH-tunnel daemon connection (config + pure forwarder foundation)

## Goal

Begin the Android companion SSH local-port-forward daemon connection mode
(`ssh -L`, Tailnet-free; Harry-requested; parity with iOS bd-caab14). Handed off
from md2-1 after a coordinated split; md2-0 advising on the connection-mode layer.
Driven as incremental slices.

## Bead(s)

- `bd-656160` — Android: SSH local port-forward daemon connection (ssh -L, no
  Tailnet) — parity with iOS bd-caab14. In progress (multi-slice feature).

## Before state

- No SSH-tunnel daemon connection mode on Android (only Tailnet/direct + the
  existing SSH-over-websocket *terminal* transport, which is distinct).

## After state (Slices 1 + 2a)

- **Slice 1 — config contract** (`connection/SshTunnelConfig.kt`): `SshTunnelConfig`
  (host/port=22/username/keyPath/remoteHost="localhost"/remotePort/localPort/
  enabled) mirroring `SshTerminalConfig`'s persisted pref pattern, plus
  `tunneledDaemonBaseUrl` (the `http://127.0.0.1:<localPort>` loopback the daemon
  client uses once the forward is up), `sshTunnelConfigComplete` validation, and
  `load/saveSshTunnelConfig`. Port defaults reference `DEFAULT_DAEMON_PORT` (never
  a port literal, per `SharedPrefsConsistencyTest`).
- **Slice 2a — pure forwarder foundation** (`connection/SshTunnelForwardSpec.kt`):
  `SshTunnelForwardSpec` (the `-L localBind:localPort:remoteHost:remotePort`
  params), `sshTunnelForwardSpec` mapping (localBind=127.0.0.1 loopback),
  `SshTunnelState` lifecycle enum, and `sshTunnelReconnectBackoffMs` (exponential,
  doubling, capped, overflow-bounded). No SSH library/sockets/threads — mirrors
  `SshKeyMaterial` S2 pure-foundation slicing.
- Validation: `gradle :app:testDebugUnitTest --tests SshTunnelConfigTest --tests
  SshTunnelForwardSpecTest --tests *SharedPrefsConsistencyTest*` → BUILD
  SUCCESSFUL (4m29s, real compile); the consistency test confirms no
  forbidden-port-literal regression.

## Diff summary

- Code commits: Slice 1 + Slice 2a (bd-656160); final landed squash SHA from the
  receipt.
- Files: `connection/SshTunnelConfig.kt`, `connection/SshTunnelForwardSpec.kt`
  (new), + pure unit tests `SshTunnelConfigTest`, `SshTunnelForwardSpecTest`.
- Tests: +6 (config defaults/completeness/base-url; forward-spec mapping; backoff
  curve). No behavioural change to existing connection modes yet (foundation).

## Operator-takeaway

The SSH-tunnel feature is landing incrementally. Slices 1+2a are the pure,
unit-testable foundation (config contract + forward params/state/backoff). The
sshj forwarder runtime (Slice 2b — `newLocalPortForwarder` + `listen()`, reusing
the existing `SshClientFactory`/`SshKeyMaterial` infra), the ConnectionManager
sibling mode (Slice 3), and the Settings UI (Slice 4) build on this. Remaining
slices are tracked in the continuity scratch note; runtime connect is
emulator-validated (deferred, mirroring the existing `openSshShell` S3b pattern).

## Remaining gaps

- Slices 2b (sshj forwarder runtime), 3 (ConnectionManager sibling mode), 4
  (Settings connection-mode UI) — designed + scoped, not yet implemented.
