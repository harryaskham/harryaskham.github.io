# Session summary — bd-a55d88 voice-call duplex session state machine

## Goal

`caco voice call <agent-id>` opens a bidirectional voice session:
operator mic → STT → on-final → msg send to target agent; agent
reply → msg-tail → TTS → speaker. This bead ships the pure-Rust
session state machine + control types that both CLI and daemon use.

## Bead(s)

- `bd-a55d88` — caco voice call duplex audio channel (P1)
- (parent epic `bd-9496d1` STT hardening)
- (depends on bd-a17114 streaming protocol — same agent, landed)

## Before state

- No voice-call session type existed. CLI/daemon would have had to
  ad-hoc the state transitions and transcript log.

## After state

- New `crates/caco-stt-protocol/src/voice_call.rs` (~500 lines)
- `CallState { Dialing, Active, Muted, Ended }` with `is_live()`
  + `is_recording()` predicates
- `EndReason` enum (6 variants): OperatorHangup, EndPhrase,
  RemoteHangup, SttError, TtsError, NetworkError
- `CallMode { PushToTalk, AlwaysOn }` (default AlwaysOn)
- `CallConfig { target_agent_id, mode, end_phrase, start_muted }`
  with builder pattern
- `TranscriptDirection { Outbound, Inbound, System }`
- `TranscriptEntry { direction, text, seq, timestamp }`
- `CallSession` state machine — pure data + transitions, no I/O:
  - `on_agent_connected()` → Dialing→Active (or Muted if start_muted)
  - `toggle_mute()` → Active↔Muted cycle; noop when Ended
  - `hangup()` → Ended (idempotent)
  - `end(reason)` → Ended with reason + system transcript
  - `on_stt_event(ev)` → returns `Some(text)` on Final for caller
    to msg-send; checks end-phrase (case-insensitive substring);
    STT Error/Stopped → session ends
  - `on_agent_reply(text)` → records inbound transcript
  - `on_agent_control("call_end")` → RemoteHangup
  - Transcript entries monotonically-sequenced

## Diff summary

- Files: 2 modified — `src/lib.rs` (+1 module decl) — and 1
  created — `src/voice_call.rs` (~500 lines incl. tests)
- Tests: +25 / -0 (caco-stt-protocol total: 86 in 0.02s)
- Behavioural delta: zero — pure addition

## Operator-takeaway

The full-session scenario test (criterion 7) drives a complete
lifecycle: dial → connect → two operator utterances → agent reply
→ mute → muted speech ignored → unmute → end-phrase → ended. It
asserts correct outbound/inbound transcript separation, seq
monotonicity, and state transitions at every step.

The CLI `caco voice call` implementation is a thin caller of this
state machine: it opens an STT stream, subscribes to the msg tail
for the target agent, and loops `on_stt_event` / `on_agent_reply`
until `CallState::Ended`. Mute toggle and volume control are
keybindings that call `toggle_mute()` and (future) a volume field.

The end-phrase check uses case-insensitive substring matching so
"okay goodbye now" or "GOODBYE" both terminate the session. The
phrase is configurable via `--end-phrase` for operators who prefer
something else.
