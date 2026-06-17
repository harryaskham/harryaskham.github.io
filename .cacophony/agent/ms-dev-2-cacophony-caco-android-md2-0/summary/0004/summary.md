# Session summary — Android pico snapshot field plumbing: notifications + transcript_ts

## Goal

Continue the storm-blocked productive code work as single owner of
PicoAgentViewSnapshot field plumbing (msd-1 owns UI): plumb the two PURE-RENDER
dropped Rust snapshot fields — recent notifications and per-message timestamps —
so msd-1 can build their UI. Field-groups (c)+(d) of the parity-audit queue;
(b) pending_dialog deferred (it needs an ExtensionUiReply send path, coordinated
separately like select_model).

## Bead(s)

- `bd-be3305` — Android pico: plumb notifications + transcript_ts into
  PicoAgentViewSnapshot (pure-render data)
- Queue siblings (landed): bd-ee47fa (availableCommands+lastSendFailure),
  bd-b9f8be (model picker)
- Deferred: (b) pending_dialog — needs the reply path (folds with bd-dd37de class)

## Before state

- PicoAgentViewSnapshot dropped the Rust snapshot's `notifications`
  (Vec<(String,String)>, kind+message) and `transcript_ts` (Vec<u64>, per-item
  unix-ms), so msd-1 could not render in-session notifications or message times.
- Failing tests: 0.

## After state

- `PicoAgentViewSnapshot.notifications: List<PicoNotification(kind, message)>`
  (new PicoNotification data class; parseNotifications parses the array-of-pairs).
- `PicoAgentViewSnapshot.transcriptTimestamps: List<Long>` (parseLongArray).
- Delivered on every snapshot, so both OkHttp + FFI sources carry it for free.
- Failing tests: 0. +2 unit tests (parse populated; defaults when absent).

## Diff summary

- Code commit: 0dc7db0173 (final landed squash SHA from the reintegration receipt).
- Files: PicoAgentView.kt (+PicoNotification, +2 fields, +fromJson parse,
  +parseNotifications/parseLongArray), PicoSessionClientSourceTest.kt (+2 tests).
- Tests: +2 / -0
- Behavioural delta: Android-only; no Rust changes; purely additive.

## Operator-takeaway

The snapshot-field plumbing is single-owner (md2-0) with msd-1 on UI, which keeps
the parity push conflict-free and pipelined. Remaining queue item (b)
pending_dialog is gated on a cross-platform interactive-reply (ExtensionUiReply)
send path the FFI doesn't expose yet — the same send-result gap aur-2 is solving
for send-failure in bd-dd37de; the dialog reply should ride the same mechanism.
