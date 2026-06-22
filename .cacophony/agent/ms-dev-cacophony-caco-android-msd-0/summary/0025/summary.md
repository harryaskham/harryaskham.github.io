# Session summary — bd-656160 Slice 2b: Android SSH-tunnel forwarder (sshj runtime)

## Goal

Add the sshj local-port-forward runtime for the Android SSH-tunnel daemon
connection mode (`ssh -L`, Tailnet-free; Harry-requested; parity with iOS
bd-caab14), on top of the landed config contract (Slice 1) + pure forwarder
foundation (Slice 2a). md2-1 handed off the design + approved Slices 1+2a;
md2-0 advising.

## Bead(s)

- `bd-656160` — Android: SSH local port-forward daemon connection. In progress
  (multi-slice; Slices 1+2a landed @859b15373f; this is Slice 2b).

## Before state

- Slices 1+2a (config + pure forward spec/state/backoff) on main; no runtime
  forwarder yet.

## After state (Slice 2b)

- `connection/SshTunnelForwarder.kt`: `openSshTunnel(config, privateKeyPem,
  policy, knownHosts)` mirrors `openSshShell` — `newSshClient` → `connect` →
  `loadKeys` → `authPublickey`, then binds a local `ServerSocket` to
  `127.0.0.1:localPort` and `client.newLocalPortForwarder(direct.Parameters(
  localBind, localPort, remoteHost, remotePort), serverSocket)`, serving the
  blocking `listen()` accept loop on a daemon thread (md2-1's threading FYI).
  `SshTunnelHandle` (Closeable) tears down in order (serverSocket.close →
  client.disconnect → thread.interrupt; closing the socket unblocks `listen()`).
- sshj 0.38.0 `direct.Parameters` API compile-verified (BUILD SUCCESSFUL,
  `compileDebugKotlin` clean). No forbidden port literals. Runtime
  connect/forward is emulator-deferred (no unit test, mirroring `openSshShell`
  S3b-connect); the pure parts (spec/state/backoff) are unit-tested in Slice 2a.

## Diff summary

- Code commit: Slice 2b (bd-656160); final landed squash SHA from the receipt.
- Files: `connection/SshTunnelForwarder.kt` (new).
- Tests: none new (runtime sshj, like `openSshShell`); compile-verified +
  existing Slice 2a/1 pure tests still green.

## Operator-takeaway

The SSH-tunnel forwarder runtime is in: the app can now (at the code level)
establish an `ssh -L` local forward to a daemon's loopback API using a configured
key, with the HTTP client targeting the forwarded local port. Remaining:
ConnectionManager sibling-mode wiring (Slice 3) + Settings UI (Slice 4), and
on-device runtime validation of the live forward (emulator, deferred like the
existing SSH terminal transport's S3b-connect).

## Remaining gaps

- Slice 3 (ConnectionManager sibling mode — base URL = `tunneledDaemonBaseUrl`
  when tunnel mode active, md2-0 advising) + Slice 4 (Settings connection-mode
  UI) + emulator runtime validation of `openSshTunnel`.
