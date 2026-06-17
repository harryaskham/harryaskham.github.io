# Session summary — Android pico per-message send-state plumbing

## Goal

Plumb aur-2's just-landed per-message send-state (bd-dd37de) into the Android
snapshot — the DISPLAY side complementing the ACT side (bd-837bb5 select_model/
sendCommand). This is the field po4's send-failure banner reads to show the REAL
network/delivery send-failure (not just the local lastSendFailure) and drive
retry.

## Bead(s)

- `bd-95431f` — Android pico: plumb transcript_send_states into
  PicoAgentViewSnapshot (per-message send-state)
- Source contract: aur-2's `bd-dd37de` (landed b4e4496df)
- Pair: `bd-837bb5` (select_model/sendCommand act path, landed 0809c01cae)

## Before state

- PicoAgentViewSnapshot dropped the Rust snapshot's `transcript_send_states`
  (Vec<Option<SendState>>), so the UI could not show real per-message
  delivery state or correct retry.
- Failing tests: 0.

## After state

- `sealed PicoSendState { Pending, Sent, Failed(reason) }` + `fromJson` (reads the
  adjacently-tagged "state": pending/sent/failed{reason}).
- `PicoAgentViewSnapshot.transcriptSendStates: List<PicoSendState?>`, index-aligned
  to `transcript` (null for non-User items / older snapshots via serde-default
  forward-compat), parsed by `parseSendStates` (JSON-null element -> null).
- Delivered on every snapshot, so both OkHttp + FFI sources carry it.
- Failing tests: 0. +2 unit tests (parse null/pending/sent/failed; default empty).

## Diff summary

- Code commit: 169341c951 (final landed squash SHA from the reintegration receipt).
- Files: PicoAgentView.kt (+PicoSendState, +transcriptSendStates field, +fromJson
  parse, +parseSendStates), PicoSessionClientSourceTest.kt (+2 tests).
- Tests: +2 / -0. Android-only (no Rust); purely additive.

## Operator-takeaway

This completes the Android side of the cross-platform send-failure feature: the
ACT path (bd-837bb5 sendCommand/selectModel) + the DISPLAY path (this) + aur-2's
host reducer (bd-dd37de). RETRY semantics matter for msd-1's render (relayed from
aur-2): retry re-sends as a NEW pending bubble (not an in-place flip); success
collapses to one delivered bubble, repeated failures stack. Needs the daemon-side
fleet roll for companions to see it live. Next: pending_dialog (its
ExtensionUiReply rides the bd-837bb5 sendCommand bridge).
