# Session summary — bd-d3f631: dropped Pico websocket disconnect proof

## Goal

Add robustness coverage for an abnormal Pico `/session` websocket close. The browser should keep the native Pico pane and already-rendered transcript visible, show a clean disconnected status, and never fall back to the terminal UI.

## Bead(s)

- `bd-d3f631` — [pico] caco-web: mock websocket scenario must cover dropped Pico session disconnect.

## Before state

- The mock scenario covered rich happy-path rendering and many outbound HostRequest classes.
- It did not cover a websocket that closes without a `pi_exited` frame.

## After state

- Added a configurable short-close mock server path.
- Added `mock_disconnect_frames()` with a single snapshot containing `This transcript should survive a dropped socket.`
- Added `run_pico_disconnect_subscenario()` to open a fresh `/pico?agent=pico-disconnect-fixture&ws=<mock>` session and let the mock socket close without `pi_exited`.
- Added `PICO_DISCONNECT_ASSERT_EVAL` to assert:
  - status text becomes `disconnected`;
  - the native Pico pane/transcript remain present;
  - the transcript text survives;
  - no terminal fallback pane appears;
  - status is not `session ended`.
- Existing full-stream and select-dialog subscenario assertions remain intact.
- Validation is green: caco-web-observe 12 tests; caco-web lib 645 tests.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-web/src/bin/caco-web-observe.rs` — configurable close delay, disconnect frames/subscenario/assertion.
  - `.cacophony/agent/.../summary/pending/web/disconnect-test/` — scenario evidence.
- Behavioural delta: no production runtime change; live browser proof now covers dropped Pico websocket behavior.

## Embedded artefacts

- `web/disconnect-test/pico-disconnect-observe.log` — scenario log with main/select/disconnect subscenario assertions.
- `web/disconnect-test/pico-disconnect-server.log` — dev server log.
- `web/disconnect-test/screenshots/*.png` — scenario screenshots.
- `web/validation.txt` — command/results summary.

## Operator-takeaway

caco-web Pico now has deterministic mock-backend proof for both happy-path structured sessions and a dropped websocket: the browser reports disconnected while preserving the native transcript instead of degrading into terminal fallback.
