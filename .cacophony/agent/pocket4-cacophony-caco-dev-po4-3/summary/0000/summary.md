# bd-a1049b: voice-attach drop affordance

## Goal
Make the bd-2cfe3e voice-attached-agent TTS filter operator-visible so the operator can tell at a glance when their voice-attach is silencing other senders, instead of perceiving it as a fleet-wide TTS outage (the bd-e251c6 reopen evidence).

## Bead(s)
- bd-a1049b — [tts/tui] Voice-attach to an agent SILENTLY drops all other agents' speeches with no operator-visible affordance (P0)
- Background: bd-e251c6 (closed works-as-designed), bd-2cfe3e (the filter introduction), bd-9e7929 (voice-attach feature)

## Before state
- `caco-tui/src/state/mod.rs:7629` and `:8996` filter SpeechRequested events: when `voice_attached_agent.is_some()`, only senders containing that agent's ID get enqueued. Everything else dropped silently — no toast, no log entry, no counter, no UI affordance beyond the tiny `🎤↔short` chip in `views/speech_indicator.rs`.
- Operator workaround was "detach from voice-call" but operator had no way to discover that voice-attach was even active.

## After state
- `SpeechState` gains `voice_attach_dropped_count: u64` and `voice_attach_drop_toasted: bool`. Both reset on every `toggle_voice_attach` call (attach, detach, switch) so accounting is per-attach-window.
- `SpeechState::note_voice_attach_drop()` increments the counter and returns `true` exactly once per window so callers can fire a one-shot toast.
- Both filter sites in `state/mod.rs` now extract the voice-attach check into an explicit `match` that calls `note_voice_attach_drop` on the drop branch and pushes a toast on first drop:
  `"🎤 Voice attached to {agent} — muting other agents' speech. Ctrl+V on that agent's detail pane to detach."`
- `views/speech_indicator.rs` shows ` muted:N` in NORD11 red next to the existing `🎤↔short` glyph whenever drops have occurred during the current attach.

## Diff summary
- `crates/caco-tui/src/speech.rs` — added 2 fields, reset hook in `toggle_voice_attach`, new `note_voice_attach_drop` method.
- `crates/caco-tui/src/state/mod.rs` — restructured both filter sites (lines ~7627 and ~9009) from boolean-chain to explicit match with drop accounting.
- `crates/caco-tui/src/views/speech_indicator.rs` — appended muted-count chip when `voice_attach_dropped_count > 0`.
- `crates/caco-tui/src/state/tests.rs` — 5 new unit tests: counter init zero, no-pending-toast, increment + one-shot semantics, multi-drop counting, reset on attach/detach/switch.
- 4 source files + 1 test file = 5 files; +129/-11 LOC excluding the summary.

## Operator-takeaway
When voice-attached to an agent (Ctrl+V on agent detail pane), the TUI now (a) fires a one-shot toast naming the attach and the detach keybind on the first muted speech from another sender, and (b) shows a persistent red `muted:N` counter in the speech indicator for the duration of the attach. Both reset on every attach/detach/switch. If you ever again think "TTS is broken", look at the speech indicator: a red `muted:N` chip means voice-attach is silencing other senders — Ctrl+V on the attached agent's detail pane to detach.

## Tests
- `cargo test -p caco-tui --lib voice_attach`: 5 new tests pass (`voice_attach_drop_counter_starts_zero_and_no_toast_pending`, `voice_attach_note_drop_increments_counter_and_returns_true_only_first_time`, `voice_attach_toggle_resets_drop_accounting`, plus the 2 pre-existing).
- `cargo test -p caco-tui --lib`: 2918 passed.
- `cargo test-small`: 187 passed.

## AC mapping
1. ✅ Persistent visible indicator naming the attached agent — existing `🎤↔short` glyph + new `muted:N` counter when drops occur.
2. ✅ Drop count visible — `muted:N` chip in NORD11 red.
3. ✅ Detach affordance — toast text names the keybind explicitly.
4. ⏭ Deferred: `caco tts status` CLI surface — out of scope for this fix; the filter is purely client-side TUI state, no daemon-side state to expose.
