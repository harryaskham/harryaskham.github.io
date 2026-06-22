# Session summary — bd-49f652: end-to-end test for happy-path Pico auto-reconnect recovery

## Goal

Harden the bounded auto-reconnect shipped in bd-dab0de with the test it was
missing: the *happy path*. The existing disconnect subscenario only proved the
budget-exhaust path (server keeps closing → 3 attempts → manual Reconnect). The
whole point of auto-reconnect — a transient drop that silently recovers to a live
session without operator action — had no end-to-end coverage. This slice adds
that proof (caco-web-observe test harness only; no product code change).

## Bead(s)

- `bd-49f652` — caco-web Pico: end-to-end test for happy-path auto-reconnect recovery
- Follows `bd-dab0de` — bounded auto-reconnect for dropped `/session` socket.

## Before state

- Failing tests: none.
- caco-web-observe `pico-pane` covered disconnect→exhaust→manual-Reconnect, but
  no test where the reconnect *succeeds* and the session returns to live.

## After state

- Failing tests: none.
- New `start_mock_pico_server_recovering` mock: drops the first `/session`
  connection (triggering auto-reconnect), keeps every subsequent connection open
  (silent recovery). New `mock_recovery_frames`, `run_pico_recovery_subscenario`,
  `PICO_RECOVERY_ASSERT_EVAL`.
- The assertion is deterministic via `cacoPicoReconnectState().attempts >= 1`
  (only reachable through a real drop + auto-reconnect; reset only after the
  stability window) rather than racily polling for the brief transient
  `reconnecting` status, which can close before the eval starts polling.
- caco-web-observe bin unit tests 12; clippy clean. Live Chromium pico-pane run
  green: recovery subscenario returns `{mode:"attached", attempts:1, isLive:true,
  hasTranscript:true, noTerminalPane:true, reconnectHidden:true}`.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/src/bin/caco-web-observe.rs` — recovering mock server,
    recovery frames/subscenario/eval, scenario registration.
- Tests: +1 live subscenario (happy-path recovery); +1 mock server variant.
- Behavioural delta: none (test-only). Proves the bd-dab0de recovery path.

## Embedded artefacts

- `web/recovery-observation.log` — live Playwright pico-pane run including the
  recovery subscenario returned object (`attempts:1`, `mode:attached`).
- `web/screenshots/*.png` — captured browser state.

## Operator-takeaway

The key testing lesson here: do not assert on a transient UI status by polling —
the `reconnecting` window can close before the test's first poll runs. The
durable signal is the reconnect *attempt count* (`cacoPicoReconnectState`), which
a real drop+reconnect raises and which only resets after the connection proves
stable. That makes the happy-path recovery test deterministic instead of flaky.
