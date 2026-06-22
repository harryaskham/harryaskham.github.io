# Session summary — bd-656160 wiring: runtime executor env + mode persistence

## Goal

Land the runtime half of the SSH-tunnel wiring — the `ConnectionModeExecutorEnv`
impl that drives the real ConnectionManager + foreground services + the
SshTunnelManagerHolder singleton, the `applyConnectionMode` bg-coroutine
entrypoint, and active-mode persistence — leaving only the Settings UI trigger
(Slice 4). Part of the Android SSH-tunnel daemon connection (`ssh -L`,
Harry-requested; iOS parity). md2-0 (connection-mode owner) designed the
(B)-singleton wiring.

## Bead(s)

- `bd-656160` — Android SSH local port-forward daemon connection. In progress
  (S1+S2a @859b15373f, S2b @23ec1020ca, S3 @7d8ab11770, wiring-core @b330b4bc41,
  FG-service @a435ef6570, executor @448b82b0ad landed; this is the runtime env).

## After state

- `connection/ConnectionModeRuntime.kt`:
  - `RuntimeConnectionModeExecutorEnv(context, connectionManager, prefs)` —
    `startTunnelAwaitReady` loads the tunnel config + key and starts the shared
    `SshTunnelManagerHolder.manager` (its synchronous bind-confirmed return = the
    ready-gate) then the keep-alive `SshTunnelForegroundService`; `stopTunnel`
    stops both; `startEmbedded`/`stopEmbedded` drive `EmbeddedDaemonForegroundService`;
    `configure` applies `tunnelConfigureArgs` (terminal-disabled loopback) for
    tunnel mode and re-applies the saved config otherwise (embedded keeps
    immediate-configure — intentional asymmetry comment).
  - `applyConnectionMode(...)` — runs `executeConnectionModeTransition(old, new)`
    on `Dispatchers.IO` (the tunnel path does network I/O), persists the new mode
    on success, and reports the result (false = tunnel ready-gate failed → caller
    surfaces a connection error). ConnectionManager is passed in (not a singleton
    — md2-0).
  - `DaemonConnectionMode` persistence (`load/saveDaemonConnectionMode` +
    pure `daemonConnectionModeFrom/ToString`), defaulting to Direct for
    null/unknown so a missing/corrupt pref never strands the app.
- Validation: `gradle :app:testDebugUnitTest` BUILD SUCCESSFUL — 3
  `DaemonConnectionModeStringTest` cases (roundtrip, default-to-Direct, known
  names) + the executor tests green. No forbidden port literals.

## Diff summary

- Code commit: runtime env (bd-656160); landed squash SHA from receipt.
- Files: `connection/ConnectionModeRuntime.kt` (new),
  `test/DaemonConnectionModeStringTest.kt` (new). Tests +3.

## Operator-takeaway

The full runtime path is now in place: a single call to `applyConnectionMode(...)`
performs the ordered, ready-gated mode transition (start tunnel → await bind →
configure → keep-alive) and persists the active mode. The only remaining piece is
the Settings UI (Slice 4): the tunnel config editor (host/user/port/key/remote/
local) + the `DaemonConnectionMode` picker that calls `applyConnectionMode`. After
that the feature is end-to-end (pending emulator runtime validation + the
multi-port ttyd/web forwarding follow-up).

## Remaining gaps

- Settings UI (Slice 4) wiring the picker/editor to `applyConnectionMode` +
  emulator runtime validation of the live tunnel + multi-port-forward follow-up
  bead.
