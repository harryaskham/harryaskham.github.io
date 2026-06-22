# Session summary — bd-656160 wiring: SSH-tunnel foreground service + manager singleton

## Goal

Land the keep-alive half of the SSH-tunnel runtime wiring — a process-level
singleton for the (stateful, live-socket) tunnel manager + a thin foreground
service that keeps the tunnel alive across backgrounding — ahead of the executor
+ mode-selection increment. Part of the Android SSH-tunnel daemon connection
(`ssh -L`, Harry-requested; iOS parity). md2-0 (connection-mode owner) designed
the (B) executor-owns + process-singleton shape.

## Bead(s)

- `bd-656160` — Android SSH local port-forward daemon connection. In progress
  (S1+S2a @859b15373f, S2b @23ec1020ca, S3 @7d8ab11770, wiring-core @b330b4bc41
  landed; this is the FG-service + singleton increment).

## After state

- `connection/SshTunnelManager.kt`: `object SshTunnelManagerHolder { val manager
  by lazy }` — the process-level single instance of `AndroidSshTunnelManager`
  (the tunnel is a stateful live socket/forward, unlike the
  stateless-over-SharedPrefs ConnectionManager that every component re-news, so
  the executor + keep-alive service share ONE instance — md2-0). The class stays
  the testable unit (fake-opener tests construct it directly).
- `connection/SshTunnelForegroundService.kt`: a thin keep-alive foreground
  service mirroring `EmbeddedDaemonForegroundService` / `RemoteMicForegroundService`
  (specialUse, START_STICKY). Per md2-0's (B) design it does NOT open the tunnel
  (the executor start()s the shared singleton first, its synchronous bind-ready
  return being the configure() gate); the service only keeps the process alive +
  surfaces status, and stops the shared manager onDestroy. Companion:
  `CHANNEL_ID="caco_ssh_tunnel"`, `NOTIF_ID=4823` (distinct from RemoteMic 4821 /
  EmbeddedDaemon 4822), pure `statusText(SshTunnelState)`, ensureChannel,
  buildNotification, start/stop.
- `AndroidManifest.xml`: registers the specialUse SSH-tunnel foreground service
  (`ssh_tunnel_forward` subtype).
- Validation: `gradle :app:testDebugUnitTest` BUILD SUCCESSFUL — 3
  `SshTunnelForegroundServiceTest` cases (statusText per state, channel/notif id
  distinct, manifest registration) + the existing manager/transition tests green.
  No forbidden port literals.

## Diff summary

- Code commit: FG-service + singleton (bd-656160); landed squash SHA from receipt.
- Files: `connection/SshTunnelForegroundService.kt` (new),
  `connection/SshTunnelManager.kt` (+holder), `AndroidManifest.xml` (service reg),
  `test/SshTunnelForegroundServiceTest.kt` (new). Tests +3.

## Operator-takeaway

The tunnel now has its keep-alive service + the shared process-singleton the
executor will drive. Next increment = the executor (run the connectionModeTransition
steps: `holder.manager.start` [bind-ready gate] → `configure(127.0.0.1:localPort)`
→ start FG service) + the connect-path mode-selection. md2-0 flagged a key
executor decision: the single tunnel forwards only the API port, so terminal
(ttyd) + web are NOT reachable in tunnel mode — disable terminal in tunnel mode
initially (preserve token + clientNodeIdentity, which travel the API) and note
multi-port forwarding as a follow-up.

## Remaining gaps

- Executor + ConnectionManager mode-selection (md2-0 PR review; terminal-disable
  decision) + Settings UI (Slice 4) + emulator runtime validation + (follow-up)
  multi-port tunnel forwarding for terminal/web.
