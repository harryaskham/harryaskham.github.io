# Session summary — bd-7ce670: Escape cancels the model-picker overlay too (TUI parity)

## Goal

Complete the Escape-cancel parity started in bd-c18a8e: the shared PendingOverlay
(bd-5f913f) covers BOTH a pending dialog and the model picker with "Esc cancel",
but bd-c18a8e only handled the dialog.

## Bead(s)

- `bd-7ce670` — Escape does not cancel the model picker overlay (only the dialog)
- Completes `bd-c18a8e` / follows `bd-5f913f`.

## Before state

- Failing tests: none. picoCancelPendingDialog only checked snap.pending_dialog,
  so when the model-picker overlay (snap.pending_model_picker) was showing,
  Escape did nothing in the web (the user had to click Cancel). The TUI cancels
  both overlays on Escape.

## After state

- Failing tests: none. picoCancelPendingDialog now also cancels a pending model
  picker (cancelPicoModelPicker() when snap.pending_model_picker is a non-empty
  array and no dialog is pending). New live subscenario: a pending model picker +
  Escape in the composer -> picker clears. 2/2 clean; bd-c18a8e static guard
  extended to pin the model-picker case.
- caco-web bin 12; `--lib` 654; clippy clean.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/app.js` — picoCancelPendingDialog also cancels the model picker.
  - `crates/caco-web/src/bin/caco-web-observe.rs` — model-picker Escape subscenario + eval.
  - `crates/caco-web/src/tests.rs` — extended the Escape-cancel guard.
- Tests: +1 live subscenario.
- Behavioural delta: Escape now cancels both the dialog and the model-picker overlay.

## Embedded artefacts

- None.

## Operator-takeaway

The bd-5f913f PendingOverlay review yielded three parity fixes (bd-6ce12c options/
title, bd-c18a8e dialog Escape, bd-7ce670 model-picker Escape). Deeply reviewing
what a peer adds to the shared layer — not just confirming tests pass — keeps
surfacing real web parity gaps under the broken-on-main monitoring duty.
