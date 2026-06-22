# Session summary — bd-e47196: bare Pico /model cycle HostRequest

## Goal

Complete caco-web Pico model command coverage by proving bare `/model` maps to the shared `cycle_model` HostRequest, complementing model picker `set_model`, `/m <provider/id>`, `/models`, and model argument autocomplete proofs.

## Bead(s)

- `bd-e47196` — [pico] caco-web: mock websocket scenario must capture bare /model cycle HostRequest.

## Before state

- The mock scenario proved model picker `set_model`, `/m <provider/id>`, `/models`, and model argument autocomplete.
- It did not submit bare `/model`, which the shared parser maps to `CycleModel`.

## After state

- The scenario submits bare `/model` through the real Pico composer.
- The mock backend asserts:
  - `{"kind":"command","type":"cycle_model"}`
- Existing model picker, `/m`, `/models`, model argument autocomplete, outbound command/reply, dialog/reconnect, render coalescing, and native display checks remain intact.
- Validation is green: caco-web-observe 12 tests; caco-web lib 646 tests.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-web/src/bin/caco-web-observe.rs` — bare `/model` submit step and outbound assertion.
  - `.cacophony/agent/.../summary/pending/web/cycle-model-test/` — scenario evidence.
- Behavioural delta: no production runtime change; live browser proof now covers model cycling command traffic.

## Embedded artefacts

- `web/cycle-model-test/pico-cycle-model-observe.log` — scenario log with received `cycle_model` frame.
- `web/cycle-model-test/pico-cycle-model-server.log` — dev server log.
- `web/cycle-model-test/screenshots/*.png` and page snapshots — scenario artifacts.
- `web/validation.txt` — command/results summary.

## Operator-takeaway

caco-web Pico now proves every main model-control path: list, cycle, set by picker, set by `/model`, and set by `/m`.
