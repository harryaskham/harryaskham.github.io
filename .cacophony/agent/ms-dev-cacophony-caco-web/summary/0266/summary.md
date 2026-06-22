# Session summary — bd-656769: system_prompt collapsible coverage (last field-matrix gap)

## Goal

Close the Pico snapshot-field live-coverage matrix: system_prompt was the last
web-read field with no live assertion. It renders as a collapsible first bubble.

## Bead(s)

- `bd-656769` — live coverage for system_prompt collapsible rendering

## Before state

- Failing tests: none. system_prompt (a collapsible <details class=
  "pico-system-prompt"> with the prompt in a <pre>) had no live test.

## After state

- Failing tests: none. New live subscenario serves a snapshot with a
  system_prompt containing an angle bracket + backtick and asserts: a collapsible
  <details> (starts collapsed), summary "system prompt", the full prompt text in
  the <pre>, and XSS-escaping (the "<helpful>" token is text, no injected
  <helpful> element). 2/2 clean.
- caco-web bin 12; `--lib` 651; clippy clean.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/src/bin/caco-web-observe.rs` — system_prompt mock + subscenario + eval.
- Tests: +1 live subscenario.
- Behavioural delta: test-only (closes the field matrix); no product change.

## Embedded artefacts

- None.

## Operator-takeaway

Every web-read Pico snapshot field now has an end-to-end live assertion
(transcript items, streaming, send-state, footer indicators, widgets/placements,
dialogs, model picker, notifications, status, and now system_prompt), backed by
the bd-b11d95 stale-wasm guard. The field-coverage matrix is complete.
