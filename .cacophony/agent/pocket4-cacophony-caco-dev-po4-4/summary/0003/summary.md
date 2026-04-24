# bd-42e5a3: daemon control integration for per-agent audio

## Goal

Complete daemon-side enforcement of the per-agent + global audio mute
hierarchy by gating the audio proxy endpoints (TTS speech + STT
transcription) so silenced agents cannot drive audio output through
the daemon's OpenAI-compatible audio surface.

## Bead(s)

- bd-42e5a3 (this commit)
- bd-46eb2b (speak handler gate, prerequisite, on main)
- bd-b2b40b (schema gate, prerequisite, on main)
- bd-677125 (read endpoint, already on main)

## Before state

`handle_speech` (POST /api/v1/audio/speech) and `handle_transcription`
(POST /api/v1/audio/transcription) honoured only the node-level
`speech.enabled` boolean (bd-55739a). A per-agent mute=true or
global_mute=true would silence message-broadcast TTS via bd-46eb2b
but the direct audio proxy endpoints would still produce/consume audio
for the silenced agent, leaving an unaudited path.

## After state

Both handlers consult `effective_speech.resolved_agent_audio(agent_id)`
right after the speech-enabled check. The agent_id is extracted from
the `x-caco-caller` header (suffix after the last `:`). When
`is_silenced()` is true:

- handler returns 409 Conflict
- error code `audio_muted`
- error message names the agent and the precedence flags
  (`global_mute`, `agent_mute`) so callers can distinguish silence
  from validation or transport failure

`handle_capabilities` is intentionally NOT gated — capability listing
is metadata about what the node can do, not an audio operation; UIs
need it to render mute-aware controls.

## Diff summary

- crates/caco-daemon/src/audio.rs: +38 lines across two handlers
  (handle_speech ~L1893, handle_transcription ~L2207). Both blocks
  identical apart from the surrounding handler context.
- No schema changes. No test fixtures changed. No new dependencies.
- `cargo check --workspace --tests`: clean.
- Resolver coverage already lives in caco-config
  (`resolved_agent_audio_per_agent_override_beats_default_agent_mute`
  and siblings, 768 lib tests pass).

## Operator-takeaway

All three daemon audio surfaces (message-broadcast speak, raw TTS
proxy, raw STT proxy) now share one mute decision via
`SpeechConfig::resolved_agent_audio`. This is the single
source-of-truth the operator wanted: regardless of which UI surface
issued the audio request, mute is enforced server-side and per-client
inconsistency is impossible.

The remaining audio cluster beads (bd-cf00b9 TUI controls, bd-7860fd
Android controls, bd-677125 web controls) are pure controllers over
the existing read+mute primitives; runtime mutation endpoints (write
new global_mute / per-agent mute via REST and persist to TOML) are a
deliberate follow-up — not in this bead's AC, not blocking the UI
surfaces because the read endpoint is sufficient for display and the
mute itself is a config change that can be applied via TOML edit +
hot-reload today.
