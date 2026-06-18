# Session summary — Android pico pending_dialog display field + Unknown parity

## Goal

Land the DISPLAY half of pending_dialog (the last pico field-plumbing item) so
msd-1 can render the dialog UI, and fold in aur-2's PicoSendState.Unknown parity
suggestion (same file). The REPLY (act) half — the 3 ui_reply FFI bindings —
follows in a separate bead for a calmer builder window (it needs a cross-language
jni_bridge.rs + cargo --features jni build).

## Bead(s)

- `bd-2151c5` — Android pico: pending_dialog DISPLAY field + PicoSendState.Unknown
- Reply (act) half: a separate follow-up bead (design in scratch
  pending-dialog-impl-prep)
- Source: Rust ExtensionUiRequest (protocol.rs L217); aur-2's Unknown FYI

## Before state

- PicoAgentViewSnapshot dropped the Rust snapshot's `pending_dialog`, so the UI
  could not render extension-UI dialogs.
- PicoSendState.fromJson mapped unrecognized states to null (no indicator),
  unlike iOS's `.unknown` neutral indicator.
- Failing tests: 0.

## After state

- `data class PicoExtensionUiRequest(id, method, rawJson)` + `isBlocking`
  (select/confirm/input/editor; fire-and-forget = notify/set_status/set_widget/
  set_title/set_editor_text).
- `PicoAgentViewSnapshot.pendingDialog: PicoExtensionUiRequest?`, parsed by
  `parsePendingDialog` (null when absent or missing id/method; rawJson = the
  `raw` field for method-specific UI rendering).
- `PicoSendState.Unknown` + `fromJson else -> Unknown` (return type now non-null
  PicoSendState) = exact iOS `.unknown` parity for future/unrecognized states.
- Failing tests: 0. +5 unit tests (dialog parse, isBlocking per method, null
  guards, Unknown state).

## Diff summary

- Code commit: a16681b660 (final landed squash SHA from the reintegration receipt).
- Files: PicoAgentView.kt (+PicoExtensionUiRequest, +pendingDialog field, +parse,
  +PicoSendState.Unknown), PicoSessionClientSourceTest.kt (+5 tests).
- Tests: +5 / -0. Android-only; purely additive (no render-side `when` on
  PicoSendState exists yet, so the new Unknown variant is safe).

## Operator-takeaway

This is the dialog DISPLAY contract; msd-1 renders the dialog + reply affordances
off `method` + `rawJson`. The REPLY act path (replyConfirm/replyValue/replyCancel
backed by 3 new PicoFfi ui_reply JNI bindings wrapping
pico_session_ui_reply_confirm/value/cancel) is the next bead — fully designed in
scratch `pending-dialog-impl-prep`, deferred to a calmer builder window because
it needs the cross-language cargo --features jni build. The Unknown fold closes
aur-2's cross-platform parity FYI.
