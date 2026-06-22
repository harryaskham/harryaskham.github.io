# Session summary — bd-dab0de: Pico bounded auto-reconnect for dropped /session socket

## Goal

Bring the web Pico session to robustness parity with the native macOS/iOS/Android
clients, which silently recover a transient socket drop. Before this, the browser
Pico `/session` socket had no auto-reconnect: any unexpected `ws.onclose` (without
`pi_exited`) dumped the operator straight to a manual `disconnected` / Reconnect
state, so a flaky network or brief daemon blip forced a hand click to recover.

## Bead(s)

- `bd-dab0de` — caco-web Pico: bounded auto-reconnect for dropped `/session` socket (native-client parity)
- Continues the Pico websocket/parity series (recent: bd-3e7417 `/think` arg autocomplete, bd-68be53 reconnect button, bd-fc1a00 connecting state).

## Before state

- Failing tests: none (baseline clean).
- `initPicoSession` opened the socket inline; `ws.onclose` went directly to
  `setPicoStatus('offline','disconnected')`. Only a manual Reconnect button
  recovered the session. No reconnecting state, no attempt budget.
- caco-web `--lib` 646; caco-web-observe bin 12.

## After state

- Failing tests: none.
- Bounded auto-reconnect: up to `PICO_MAX_RECONNECT_ATTEMPTS` (3) automatic
  reconnects with short increasing backoff (500/1000/1500 ms), visible
  `reconnecting… (N/3)` status with a new `agent-pico-dot--reconnecting` pulse,
  transcript preserved across attempts, then fallback to manual `disconnected`
  once exhausted. The budget refreshes only after a socket stays open past
  `PICO_RECONNECT_STABLE_MS` (3 s), so an immediately-reclosing server still
  exhausts deterministically (no infinite loop). Manual Reconnect resets the
  budget; `pi_exited`/ended sessions never auto-reconnect.
- New `openPicoSocket`, `handlePicoSocketClose`, `clearPicoReconnectTimers`,
  and a `window.cacoPicoReconnectState()` getter for DevTools/tests.
- caco-web `--lib` 646 (incl. bd-68be53/bd-931b16/bd-0525ab guards); clippy
  clean; bin 12. Live Chromium pico-pane run green.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/app.js` — `picoState` reconnect fields +
    `PICO_MAX_RECONNECT_ATTEMPTS`/`PICO_RECONNECT_STABLE_MS`; `openPicoSocket`,
    `handlePicoSocketClose`, `clearPicoReconnectTimers`,
    `window.cacoPicoReconnectState`; teardown/init/manual-reconnect timer + budget
    handling.
  - `crates/caco-web/static/style.css` — `.agent-pico-dot--reconnecting`.
  - `crates/caco-web/src/bin/caco-web-observe.rs` — rewrote the disconnect
    subscenario assert to verify bounded auto-reconnect via
    `cacoPicoReconnectState()` (sawReconnecting, attempts===max, budget reset on
    manual reconnect).
  - `crates/caco-web/src/tests.rs` — allowlisted `cacoPicoReconnectState`.
- Tests: reworked 1 live subscenario (now asserts auto-reconnect); +1 window
  export allowlist entry.
- Behavioural delta: dropped Pico sockets now self-heal up to 3× with a visible
  reconnecting indicator before requiring manual intervention.

## Embedded artefacts

- `web/reconnect-observation.log` — live Playwright pico-pane run including the
  disconnect subscenario returned object (attempts 3/3, budget reset to 0,
  re-exhaust to 3/3).
- `web/screenshots/*.png` — captured browser state.

## Operator-takeaway

The browser Pico session now recovers transient drops on its own, matching the
native clients, and only surfaces the manual Reconnect button after a bounded
budget is spent. The stabilization-window design (refresh the budget only after a
socket proves stable) is the key detail: it keeps auto-reconnect from looping
against a server that keeps closing, while still giving a healthy reconnected
session a fresh budget for the next real drop.
