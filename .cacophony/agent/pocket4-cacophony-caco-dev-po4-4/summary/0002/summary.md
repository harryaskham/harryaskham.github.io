# bd-46eb2b: per-agent + global audio mute routing in daemon speak handler

## Goal

Wire the AudioGlobals + per-agent ResolvedAgentAudio precedence (landed
in bd-b2b40b) into the daemon's `handle_msg_speak` so a silenced agent's
narration messages no longer emit TTS-bound feed events or audio.

## Bead(s)

- bd-46eb2b (this commit)
- bd-b2b40b (schema gate, prerequisite, already on main)

## Before state

`handle_msg_speak` honoured only the test-agent isolation early-return
(bd-ec95d1). When a real agent had `mute=true` in `PerAgentAudioConfig`
or when `AudioGlobals.global_mute=true`, the speak handler still emitted
a `MessageSpeak` feed event, fanned out to peers, and registered TTS ack
waiters. Mute was honoured client-side only, which meant per-listener
inconsistency and wasted TTS work.

## After state

`handle_msg_speak` consults `state.config.speech.resolved_agent_audio()`
right after the test-agent gate. When `resolved.is_silenced()`:

- the message is still persisted via `MessageStore::insert` (inbox/
  history visibility preserved)
- the speak stamp file is still written (hook enforcement preserved)
- `agents.record_speak` + `record_authoritative_caller_activity` still
  fire (liveness preserved)
- the feed event, replicator fan-out, and TTS ack waiter registration
  are skipped
- the response includes `audio_muted: true` plus `global_mute` and
  `agent_mute` booleans so callers can distinguish silence from
  delivery failure

## Diff summary

- crates/caco-daemon/src/lib.rs: +46 lines in `handle_msg_speak`
  (single early-return block, mirrors bd-ec95d1 test-agent pattern).
- No other files touched.
- `cargo check --workspace --tests` clean.
- `cargo test -p caco-config --lib` 768/768 pass (no regression on the
  schema layer landed in bd-b2b40b).

## Operator-takeaway

Daemon is now the single source of truth for audio-silence enforcement.
TUI/web/Android per-agent mute UIs (bd-cf00b9 / bd-7860fd / bd-677125)
become pure controllers over `PerAgentAudioConfig` / `AudioGlobals`;
they no longer need to filter feed events themselves. This unblocks
the rest of the audio cluster and removes a class of "muted on one
client, audible on another" inconsistency.

Follow-up: a direct unit test for the silenced early-return path is
deferred (handler tests need a `DaemonState` fixture; the existing
test-agent isolation test in `agent/tests.rs` is the template). Filed
mentally as a small follow-up bead candidate.
