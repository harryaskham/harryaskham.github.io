# Session summary — bd-656160 wiring (core): connection-mode transition ordering

## Goal

Land the pure core of the SSH-tunnel wiring slice — the daemon connection-mode
model + the ordered keep-alive transition plan encoding md2-0's teardown/ready-gate
ordering — ahead of the runtime foreground-service + executor wiring (which builds
on it). Part of the Android SSH-tunnel daemon connection (`ssh -L`, Harry-requested;
iOS parity). md2-0 (connection-mode owner) designed + endorsed the ordering.

## Bead(s)

- `bd-656160` — Android SSH local port-forward daemon connection. In progress
  (Slices 1+2a @859b15373f, 2b @23ec1020ca, 3 @7d8ab11770 landed; this is the
  pure wiring core).

## Before state

- `AndroidSshTunnelManager` (Slice 3) + `openSshTunnel` (Slice 2b) landed; no
  connection-mode model and no transition ordering for switching modes.

## After state

- `connection/ConnectionMode.kt`: `DaemonConnectionMode {Direct, Embedded,
  SshTunnel}` + `ConnectionModeStep {StopTunnel, StopEmbedded,
  StartTunnelAwaitReady, StartEmbedded, Configure}` + pure
  `connectionModeTransition(old, new): List<ConnectionModeStep>`.
- Invariants (md2-0): on a mode SWITCH the old keep-alive is torn down BEFORE
  `Configure` (no orphan tunnel/foreground service); a transition INTO SshTunnel
  puts `StartTunnelAwaitReady` BEFORE `Configure` (Option A ready-gate — the
  forward must be bound+listening before the client targets it, since the tunnel's
  SSH-handshake startup is slower than the embedded daemon's ~ms in-process start,
  so immediate-configure would flash a connection error); embedded keeps
  immediate-configure (negligible race; the asymmetry is intentional); a same-mode
  reconnect does not tear down the running keep-alive (idempotent start).
- Validation: `gradle :app:testDebugUnitTest` BUILD SUCCESSFUL — 9
  `ConnectionModeTransitionTest` cases (every transition pair + two ordering
  invariants: StopTunnel-before-Configure, StartTunnelAwaitReady-before-Configure);
  Slice 3 `SshTunnelManagerTest` green. No forbidden port literals.

## Diff summary

- Code commit: wiring core (bd-656160); final landed squash SHA from the receipt.
- Files: `connection/ConnectionMode.kt` (new), `test/ConnectionModeTransitionTest.kt`
  (new). Tests +9.

## Operator-takeaway

The connection-mode transition ordering — the subtle correctness bit md2-0
flagged (no orphan foreground service on switch; tunnel ready-gate before
re-pointing the client) — is now a pure, fully-tested function. The runtime
wiring that consumes it (`SshTunnelForegroundService` + a bg-coroutine executor
that runs the steps: stop-old → start-new (await tunnel ready) → configure) +
the `DaemonConnectionMode` selection is the next increment, which md2-0 reviews.

## Remaining gaps

- `SshTunnelForegroundService` + the executor + ConnectionManager mode-selection
  (md2-0 PR review) + Settings connection-mode UI (Slice 4) + emulator runtime
  validation of `openSshTunnel`.
