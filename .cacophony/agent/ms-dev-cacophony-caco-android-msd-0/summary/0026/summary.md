# Session summary — bd-656160 Slice 3: Android SSH-tunnel lifecycle manager

## Goal

Add the SSH-tunnel lifecycle manager + the effective-endpoint derivation for the
Android SSH-tunnel daemon connection mode (`ssh -L`, Tailnet-free; Harry-requested;
parity with iOS bd-caab14), on top of landed Slices 1+2a (config + pure
foundation) + 2b (sshj forwarder runtime). md2-0 (connection-mode layer owner)
reviewed the design; md2-1 approved Slices 1+2a+2b.

## Bead(s)

- `bd-656160` — Android: SSH local port-forward daemon connection. In progress
  (Slices 1+2a @859b15373f, 2b @23ec1020ca landed; this is Slice 3).

## Before state

- sshj forwarder runtime (Slice 2b) landed; no manager owning its lifecycle, and
  no derivation of the ConnectionManager endpoint for tunnel mode.

## After state (Slice 3)

- `connection/SshTunnelForwardSpec.kt`: `SshTunnelDaemonEndpoint(host, port,
  mtlsEnabled)` + `sshTunnelDaemonEndpoint(config)` → the loopback endpoint
  (127.0.0.1:localPort, mtls=false) the ConnectionManager is `configure()`-d with
  when tunnel mode is active. Per md2-0: the tunnel carries the remote node's
  plain-http LOCAL bearer-token API (not the mTLS cluster API), so mtls is always
  false and the base token is the remote node's local-API token.
- `connection/SshTunnelManager.kt`: `AndroidSshTunnelManager` — a sibling of
  `AndroidEmbeddedDaemonManager` owning the forward's open/close lifecycle, with
  the sshj connect behind an injectable `SshTunnelOpener` seam (`RealSshTunnelOpener`
  → `openSshTunnel`), exactly mirroring the embedded daemon's `EmbeddedDaemonRuntime`
  seam. `@Synchronized start(config, privateKeyPem)` opens the tunnel + returns the
  endpoint (idempotent when already running; null+Failed on incomplete config or
  open failure); `stop()` closes the handle. Does NOT touch ConnectionManager (per
  md2-0, tunnel lifecycle lives OUTSIDE the SSE-only, all-modes-shared
  `disconnect()`).
- Validation: `gradle :app:testDebugUnitTest` BUILD SUCCESSFUL — 6 new
  `SshTunnelManagerTest` cases (open→endpoint, mtls=false, incomplete→Failed,
  open-throws→Failed, stop→closes+Disconnected, idempotent start) via a fake
  opener; existing tunnel + `SharedPrefsConsistencyTest` green. No forbidden
  port literals.

## Diff summary

- Code commit: Slice 3 (bd-656160); final landed squash SHA from the receipt.
- Files: `connection/SshTunnelManager.kt` (new), `connection/SshTunnelForwardSpec.kt`
  (+endpoint helper), `test/SshTunnelManagerTest.kt` (new). Tests +6.

## Operator-takeaway

The tunnel now has a lifecycle manager (open/close + state) that yields the exact
endpoint to point the daemon client at, with the runtime sshj connect behind a
unit-testable seam. Remaining: the ConnectionManager mode-selection wiring (start
manager → `configure()` with the derived endpoint → stop on switch) + a foreground
service to keep the tunnel alive while backgrounded (md2-0's design point,
mirroring `EmbeddedDaemonForegroundService`) + Settings UI (Slice 4). The manager
is intentionally a plain `@Synchronized` lifecycle object (not Activity-scoped) so
a foreground service can hold it.

## Remaining gaps

- Wiring slice: ConnectionManager mode-selection + `SshTunnelForegroundService`
  keep-alive (md2-0 to review the PR). Slice 4: Settings connection-mode UI.
  Emulator runtime validation of `openSshTunnel`.
