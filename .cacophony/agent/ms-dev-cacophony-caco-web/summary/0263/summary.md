# Session summary — bd-175950: malformed-frame resilience live test

## Goal

Verify the Pico pane survives a malformed/garbage WebSocket frame mid-session
(robustness). The onmessage path defends in two layers (applyPicoSharedLine wraps
the wasm apply_line in try/catch with a legacy-reducer fallback; the fallback
wraps JSON.parse in try/catch), but there was no end-to-end test that a garbage
frame is actually survived.

## Bead(s)

- `bd-175950` — live test that a malformed WebSocket frame mid-session does not break the pane

## Before state

- Failing tests: none. The malformed-frame defenses were source-verified only;
  no test proved a garbage frame is swallowed without breaking the live pane.

## After state

- Failing tests: none. New live subscenario sends a valid snapshot (Assistant
  "alpha message"), then a raw NON-JSON garbage frame (MockPicoFrame with a
  hand-built non-JSON line), then a second valid snapshot ("alpha message" +
  "beta message"). Asserts BOTH messages render, the garbage text does NOT, and
  the status stays live (not offline/disconnected/connection failed). 2/2 clean.
- caco-web bin 12; `--lib` 651; clippy clean.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/src/bin/caco-web-observe.rs` — mock (with a raw garbage frame) + resilience subscenario + eval.
- Tests: +1 live subscenario.
- Behavioural delta: test-only (robustness); no product change.

## Embedded artefacts

- `web/malformed-run.log` — clean run incl. the resilience result.

## Operator-takeaway

Confirms the two-layer malformed-frame defense works end-to-end: a garbage frame
is swallowed (wasm apply_line catch + JSON.parse catch) and the pane keeps
processing valid frames without dropping the session. With streaming-diff
correctness (bd-0e6184), the field-coverage audit, and this resilience test, the
web Pico WebSocket path is covered for performance, correctness, and robustness.
