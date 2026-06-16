# Session summary — Android pico /session websocket robustness (bd-bcc919)

## Goal

Fix the Android companion's local Pico agent connection getting stuck at
"connecting / no session state". The daemon-side `/session` proxy EOF was
already fixed upstream (bd-4c9275, v1.2.1239+); this session delivers the
remaining Android-app-side robustness slice so a hung upgrade or a
dead-but-open mobile socket surfaces a clear failure instead of silently
wedging the UI.

## Bead(s)

- `bd-bcc919` — Fix Android app Pico agent connection via daemon socket proxy (P1 bug)

## Before state

- Failing tests: none (pre-existing green).
- Android already routed pico sessions correctly: `DaemonConfig.picoSessionWebSocketUrl`
  builds `ws://host:11100/api/v1/agents/{id}/session` (the daemon socket proxy),
  used by `ConnectionManager.picoSessionConnection` → `OkHttpPicoSessionSource`.
- Robustness gap: `createPicoSessionSource()` reused `sseClient`, which has
  `readTimeout(0)` (infinite) and NO `pingInterval`. So a proxy that accepts the
  TCP connection but never returns HTTP 101 hung in "Connecting" forever, and a
  half-open/idle-dropped socket after upgrade was never detected → UI stuck with
  no session state. `PicoStandaloneActivity` used a bare `OkHttpClient()` with no
  ping either.

## After state

- Failing tests: none. New `PicoSessionRobustnessTest` (3) + extended
  `PicoSessionClientSourceTest` (now 6) all green; full `:app:testDebugUnitTest`
  build SUCCESSFUL on ms-dev-2 in the Android Nix devshell.
- Dedicated pico `/session` websocket client: `connectTimeout(10s)` +
  `readTimeout(30s)` (bounds ONLY the handshake — OkHttp sets the websocket
  SO_TIMEOUT to 0 after a successful upgrade, so this never kills an idle-but-
  healthy open session) + `pingInterval(20s)` (keepalive + dead-peer detection).
- `ConnectionManager` builds it via `buildPicoWebSocketClient(mtlsEnabled)`,
  rebuilt on `configure` so mTLS material is honored; `createPicoSessionSource()`
  now uses it instead of `sseClient`. `PicoStandaloneActivity` uses the shared
  `OkHttpPicoSessionSource.defaultWebSocketClient()` factory.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `connection/ConnectionManager.kt` — dedicated `picoWebSocketClient` field +
    `buildPicoWebSocketClient`, rebuilt in `applyHttpClientsForConfig`;
    `createPicoSessionSource` uses it.
  - `ui/pico/PicoSessionClient.kt` — `OkHttpPicoSessionSource.defaultWebSocketClient()`
    factory + ping/handshake-timeout constants.
  - `ui/pico/PicoStandaloneActivity.kt` — use the factory; drop bare `OkHttpClient()`.
  - test `PicoSessionClientSourceTest.kt` — +1 contract-pin test.
  - test `PicoSessionRobustnessTest.kt` — new: config assertion + 2 MockWebServer
    behavioral tests (NO_RESPONSE hung upgrade → Failed; non-upgrade 200 → Failed).
- Tests: +4 (1 source pin, 3 robustness), -0, flipped 0.
- Behavioural delta: a stalled or dead pico session now transitions to `Failed`
  within the bounded handshake timeout / ping window instead of hanging in
  `Connecting` / showing no session state.

## Embedded artefacts

- None. This is a non-visual transport-layer change; emulator screenshots would
  not exercise the changed path (no local pico agent is registered against the
  emulator's daemon), so validation is the deterministic Gradle unit + MockWebServer
  behavioral suite rather than a UI capture.

## Operator-takeaway

The Android routing was already correct; the real bug class was an unbounded
websocket handshake + no keepalive, which let any daemon/proxy stall wedge the
pico UI silently. The fix is defensive and independent of the daemon-side EOF
fix: even a future proxy hang now surfaces as a clear `Failed` state the user
can retry. The MockWebServer `NO_RESPONSE → Failed` test is the durable guard
against this regressing — exactly the kind of API-boundary contract test that
catches "stuck forever" invariants that unit tests miss.
