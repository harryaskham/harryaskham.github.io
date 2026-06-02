# Session summary — VoiceCallRouter finalized-session leak fix (bd-c65f31)

## Goal

Fix a confirmed latent correctness gap in the voice-call orchestration
router: finalized sessions were never removed from the router's session
map, so once the (currently unwired) `VoiceCallRouter` is integrated into
a long-lived holder it would leak a full call session per finished call
and permanently reject reuse of a finalized session id. The aim was the
minimal correct fix plus a regression test, with zero behavioural change
to the still-sound underlying call-session state machine.

## Bead(s)

- `bd-c65f31` — VoiceCallRouter never removes finalized sessions from
  self.sessions map (latent leak + session-id-reuse rejection; unwired
  scaffolding). P3 bug, labels: draft, latent, stt, voice-call.
- (context: parent voice-call epic `bd-9496d1` (permanent); the
  scaffolding's adding task `bd-07d590` is already closed, so this gap
  shipped uncaught. Filed by aur-1 during voice-call review and left
  open/unassigned per reflect-session "don't self-claim your own draft"
  discipline; picked up here as ordinary non-specialist dev-queue work.)

## Before state

- Failing tests: none (pre-existing baseline green for the module).
- `crates/caco-stt-protocol/src/voice_call_orchestration.rs`:
  `self.sessions` was insert-only — single insert in `start_session`
  (~line 351), no remove/retain/clear/prune anywhere. `finalize_session_actions`
  removed only from `live_by_surface`, leaving the `RoutedVoiceCallSession`
  entry permanently.
- `VoiceCallRouter` confirmed unwired (workspace grep finds no consumer
  outside its own module/tests) → zero live runtime impact today.
- Module test count: 20 tests in `voice_call_orchestration::tests`.

## After state

- Failing tests: none. `cargo test -p caco-stt-protocol voice_call_orchestration`
  → 21 passed / 0 failed (108 filtered out). `cargo clippy -p
  caco-stt-protocol --all-targets -- -D warnings` clean.
- `finalize_session_actions` now also calls `self.sessions.remove(session_id)`
  alongside the existing `self.live_by_surface.remove(&surface)`, freeing
  both indexes on finalize. The `FinalizeTranscript` action already carries
  the rendered markdown out, so the in-map entry is not needed post-finalize.
- New regression test `router_removes_finalized_session_and_allows_session_id_reuse`
  asserts the leak fix (`get()` is None + `active_session_count()` 0 after
  finalize) and the reuse fix (same session_id reusable after finalize).
- Module test count: 21 tests.

## Diff summary

- Code/content commit: `eda1740246` (final landed squash SHA will come from
  the reintegration receipt).
- Files touched: `crates/caco-stt-protocol/src/voice_call_orchestration.rs`
  (+62 lines: one-line removal in `finalize_session_actions` with an
  explanatory comment, plus the new regression test).
- Tests: +1 (`router_removes_finalized_session_and_allows_session_id_reuse`).
- Behavioural delta: finalized voice-call sessions are dropped from the
  router session map on finalize, fixing an unbounded-growth leak and a
  permanent session-id-reuse rejection that would surface only once the
  router is wired into a long-lived holder. No change to the call-session
  state machine or to live/active routing behaviour.

## Operator-takeaway

A latent (unwired-scaffolding) leak in `VoiceCallRouter` is now closed with
a one-line map cleanup plus a regression test, so the voice-call epic
(bd-9496d1) can wire the router into a long-lived daemon holder without
inheriting unbounded session-map growth or permanent session-id-reuse
rejection. aur-1's separate review independently confirmed the underlying
call-session state machine is sound, so the router-layer fix is the
complete fix for this gap.
