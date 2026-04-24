# Session summary — bd-07d590 voice-call orchestration extras

## Goal

Operator → controller live voice call: handoff signals (chimes),
direct-DM (not broadcast), voice-call-mode prompt hint for
controller-class agents, and savable transcript file. Builds on
bd-a55d88's CallSession.

## Bead(s)

- `bd-07d590` — voice-call orchestration (P1, operator-asked)
- (parent epic `bd-9496d1` STT hardening; combines with bd-a55d88)

## Before state

- bd-a55d88 shipped CallSession state machine but no
  orchestration: no chime markers, no DM envelope helper, no
  controller-mode hint, no transcript file format.

## After state

- New `crates/caco-stt-protocol/src/voice_call_orchestration.rs`
  (~360 lines)
- `HandoffChime { Start, End, Error, Mute, Unmute }` with stable
  `asset_key()` (`call-start`, `call-end`, `call-error`,
  `mute-on`, `mute-off`)
- `end_chime_for(reason)` — picks End vs Error based on EndReason
- `DmEnvelope { to_agent_id, body, session_id, voice_call }` —
  contract for direct-DM `caco msg send` (criterion 2: NEVER
  broadcast)
- `build_dm_envelope(session, session_id, body)` — single source
  of truth for envelope construction
- `VOICE_CALL_MODE_HINT` constant — terse-mode profile snippet
  the daemon prepends to controller prompts (criterion 9)
- `voice_call_mode_hint(session)` — Option<&str> per session-live
  state for ergonomic `unwrap_or_default()` in callers
- `render_transcript_markdown(session, session_id)` — pure-string
  markdown formatter for criterion-4 file save

## Diff summary

- Files: 2 modified — `src/lib.rs` (+1 mod) + 1 created
  (`voice_call_orchestration.rs`)
- Tests: +12 / -0 (caco-stt-protocol total: 120 in 0.02s)
- Behavioural delta: zero — pure addition

## Acceptance status

- [x] Criterion 1: chime-in / chime-out via HandoffChime::{Start,
  End, Error}
- [x] Criterion 2: DmEnvelope.to_agent_id is concrete (no
  wildcard); voice_call=true marker for routing
- [ ] Criterion 3: TTS playback wiring — daemon-side, requires
  TTS engine pick (not in protocol crate)
- [x] Criterion 4: render_transcript_markdown produces
  saveable markdown
- [x] Criterion 5: bd-a55d88 already shipped PushToTalk/AlwaysOn
- [x] Criterion 6: bd-a55d88 already shipped toggle_mute()
- [x] Criterion 7: bd-a55d88 already shipped end-phrase + hangup
- [x] Criterion 8: full-orchestration scenario test exercises
  connect → speak → DM envelope → reply → hangup → end chime →
  markdown save
- [x] Criterion 9: VOICE_CALL_MODE_HINT directs terse + no
  markdown + no goodbye-narration

## Operator-takeaway

Voice-call orchestration is now end-to-end at the protocol layer.
The full-scenario test drives every piece:

```rust
let mut s = CallSession::new(CallConfig::new("ctrl-1"));
s.on_agent_connected();                                 // +Start chime
let body = s.on_stt_event(&final_event("status?"))?;
let env = build_dm_envelope(&s, "sess-99", body);       // direct DM
s.on_agent_reply("Fleet healthy");                      // → TTS
s.hangup();
let chime = end_chime_for(s.end_reason.as_ref().unwrap()); // End chime
let md = render_transcript_markdown(&s, "sess-99");     // savable
```

CLI-side wiring left:
- play `chimes/<asset_key>.wav` on each HandoffChime (5 keys)
- POST DmEnvelope as `caco msg send` (criterion 2)
- prepend VOICE_CALL_MODE_HINT to controller prompt for session
  duration (criterion 9)
- write `render_transcript_markdown(...)` to
  `$XDG_DATA_HOME/caco/voice-calls/<session-id>.md` on hangup

Criterion 3 (TTS playback) is the only piece that needs
non-protocol work — it depends on a TTS engine pick, which is
not in this crate's scope.
