# Session summary — bd-c18a8e: Escape cancels a pending Pico dialog (TUI parity)

## Goal

Close a keyboard-a11y / native-parity gap found by reviewing bd-5f913f's render.rs:
the shared TUI PendingOverlay shows "Esc cancel" for a pending dialog, but the web
had no Escape-to-cancel (only a Cancel button).

## Bead(s)

- `bd-c18a8e` — Escape does not cancel a pending dialog (TUI shows 'Esc cancel'; web only has the Cancel button)
- Found reviewing peer land `bd-5f913f` (shared PendingOverlay with the Esc-cancel hint).

## Before state

- Failing tests: none. picoComposerKey handled Escape only to dismiss the
  suggestion popup (when suggestions.length); with a blocking dialog pending and
  no suggestions, Escape did nothing. The TUI cancels on Escape.

## After state

- Failing tests: none. New picoCancelPendingDialog() helper answers the
  pending_dialog with 'cancel'. picoComposerKey now calls it on Escape when no
  suggestion popup is consuming the key (composer focus); a picoPanelKey keydown
  listener bound on the transcript host calls it too (covers focus in a dialog
  input field, which lives in the transcript). New live subscenario: a pending
  confirm dialog + Escape in the composer -> dialog clears (cancelled), 2/2 clean;
  static guard pins the full wiring incl. the transcript-host binding.
- caco-web bin 12; `--lib` 654 (+1 guard); clippy clean.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/app.js` — picoCancelPendingDialog + picoPanelKey + picoComposerKey Escape + host keydown binding.
  - `crates/caco-web/src/bin/caco-web-observe.rs` — Escape-cancel dialog subscenario + eval.
  - `crates/caco-web/src/tests.rs` — static guard for the Escape-cancel wiring.
- Tests: +1 live subscenario, +1 static guard.
- Behavioural delta: Escape now cancels a pending Pico dialog, matching the TUI.

## Embedded artefacts

- None.

## Operator-takeaway

The broken-on-main monitoring duty (gate off + active peers) keeps finding
parity work: bd-5f913f's shared PendingOverlay revealed the TUI's Esc-cancel
behavior the web lacked. Pico dialogs are now Escape-cancellable from both the
composer and the dialog input, matching native.
