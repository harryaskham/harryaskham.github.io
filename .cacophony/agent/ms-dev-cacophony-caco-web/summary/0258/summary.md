# Session summary — bd-b11d95: guard committed wasm against stale snapshot schema

## Goal

Prevent the stale-wasm regression class found in bd-b357f0 from recurring: when
the shared caco-picophony AgentViewSnapshot schema gains a field but the
committed pico_view_bg.wasm is not regenerated, the web silently drops the field.

## Bead(s)

- `bd-b11d95` — guard committed pico_view_bg.wasm against stale snapshot schema
- Follows `bd-b357f0` (the widget_placements stale-wasm bug this guards against).

## Before state

- Failing tests: none. No test verified the committed wasm actually carries the
  snapshot fields the web reads; the bd-b357f0 stale wasm (widget_placements
  count 0) passed all existing guards while the feature was dead end-to-end.

## After state

- Failing tests: none. New caco-web lib guard
  `pico_view_wasm_carries_current_snapshot_fields_bd_b11d95` reads the embedded
  pico_view_bg.wasm and asserts it carries, as serde field-name strings, the
  15 snapshot fields the frontend reads, plus that app.js has a live consumer of
  widget_placements. A stale wasm now fails HERE with a regenerate message
  instead of silently breaking in the browser.
- caco-web `--lib` 650 (was 649); clippy clean.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/src/tests.rs` — new stale-wasm guard test.
- Tests: +1 guard.
- Behavioural delta: test-only (CI robustness); no product change.

## Embedded artefacts

- None.

## Operator-takeaway

This is the systemic complement to the bd-b357f0 lesson. A static source guard
proves the JS code exists but not that the shared wasm carries the field; this
guard closes that gap for every web-read snapshot field at once. If a future
commit changes the caco-picophony snapshot schema without `just pico-web-wasm`,
the committed wasm goes stale and this test fails loudly.
