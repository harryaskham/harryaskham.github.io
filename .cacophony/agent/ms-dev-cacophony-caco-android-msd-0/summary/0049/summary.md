# Session summary — bd-376ea1 Wear OS SSH-tunnel, Slice 3 (on-watch sshj forwarder)

## Goal
Continue bd-376ea1 (Wear OS SSH port-forward daemon connection, parity of phone
bd-656160). Slices 1, 2a, 2b, 2c, 2d landed (the full phone<->watch key-transfer
round-trip). This reintegration is Slice 3: the on-watch sshj forwarder that
actually opens the SSH local-port-forward, plus TOFU host-key verification.

## What landed this reintegration (Slice 3 — on-watch sshj forwarder)
- `companion/android/wearable/build.gradle.kts`: added `com.hierynomus:sshj:0.38.0`
  + `org.bouncycastle:bcprov-jdk18on:1.75` (impl) + `org.apache.sshd:sshd-core:2.12.1`
  (testImpl) — mirrors the phone app's SSH deps (version-matched bcprov, no new weight).
- NEW `wearable/.../wear/connection/WatchSshKnownHosts.kt` — trust-on-first-use (TOFU)
  host-key verification ported watch-local from the phone's SshKnownHosts.kt:
  `watchSshTofuDecision`/`watchSshKnownHostKey` (pure), `WatchTofuHostKeyVerifier`
  (sshj HostKeyVerifier), `WatchSshKnownHostsStore` (SharedPreferences). Pins the
  daemon host key on first connect, rejects later changes (MITM defense).
- NEW `wearable/.../wear/connection/WatchSshTunnelForwarder.kt` — the sshj runtime,
  ported watch-local from the phone's SshTunnelForwarder.kt + SshClientFactory.kt:
  `ensureWatchSshSecurityProvider` (registers the full BouncyCastle provider once —
  Android's stripped "BC" lacks X25519 for curve25519 KEX), `newWatchSshClient`,
  `watchTofuHostKeyVerifier(context)`, `openWatchSshTunnel(config, pem, verifier)`
  (connect + publickey-auth + ONE LocalPortForwarder for the daemon API port —
  watch-constrained, no ttyd/web — served on a daemon thread) returning a
  `WatchSshTunnelSession{boundLocalPorts; close()}`, and `openWatchSshTunnelFromStore`
  (reads the PEM via WatchSshTunnelKeyStore.load + TOFU). cfg endpoint reuses slice-1
  `watchSshTunnelForwardSpec`/`watchSshTunnelDaemonEndpoint`.
- NEW `WatchSshTunnelForwarderTest.kt` — TOFU decision unit tests + an in-process
  Apache MINA sshd integration test (AcceptAllForwardingFilter): connect through the
  bound local port, assert real bytes (TUNNEL_OK) forward end to end, boundLocalPorts
  contains the local port, and the port is released after close().

## Next slices
(4) WatchConnectionManager connect-seam (on tunnel-up set WatchConnectionConfig from
watchSshTunnelDaemonEndpoint so md2-0's terminal/assistant surfaces route through the
tunnel transparently), (5) WatchSettings UI, (6) foreground/runtime-limit service.

## Validation
Queued `:wearable:testDebugUnitTest` (5 Watch SSH classes incl. the new TOFU +
forwarder integration test, tj-ccc9f156) PASSED — confirms sshj+BC resolve + compile
in the wearable module and the forwarder mechanics work. The DEX-on-Wear compat of
sshj/BouncyCastle is additionally validated by the assembleRelease reintegration gate.
No forbidden port literals. Reint gate (assembleRelease) is the real validation of record.

## Diff
See the reintegration receipt for the final landed squash SHA.
