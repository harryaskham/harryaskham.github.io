# Session summary — realtime speech-to-speech turn path

## Goal

Implement the first true realtime speech-to-speech slice for `bd-70c326`: expose a daemon and CLI path that sends input audio over the OpenAI-compatible realtime WebSocket as `input_audio`, receives synthesized output audio, and documents how this differs from transcription-only STT or text-to-audio realtime turns.

## Bead(s)

- `bd-70c326` — Enable true speech-to-speech over realtime model WebSockets

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: `bd-c98fa3` had just landed the model catalog additions for `gpt-realtime-2` and `gpt-realtime-whisper`.
- Context: the existing realtime command was `caco audio realtime-turn --text`, which used the realtime WebSocket but sent a text conversation item; there was no first-party audio-input realtime command/API path.

## After state

- Failing tests: none from targeted validation. Peer-owned broken-on-main caco-tui kitty animation failures were acknowledged and not duplicated.
- Relevant metrics: docs page validation passed; targeted queued tests passed for daemon realtime input-audio item generation and CLI realtime-speech command metadata.
- Context: new `POST /api/v1/audio/realtime/speech` accepts base64 input audio, validates realtime model/voice/provider availability, sends a realtime `input_audio` conversation item, and returns WAV output/audio metadata. New `caco audio realtime-speech --file ...` wraps that endpoint with `--output`/`--stdout`, model/voice/instructions/transcription-model flags, and generated MCP metadata.

## Diff summary

- Commits: `79559ac72f` (implementation commit after first-party rebase; final landed squash SHA to be assigned by reintegration receipt).
- Files touched: `README.md`, `crates/caco-cli/src/audio_cmd.rs`, `crates/caco-cli/src/lib.rs`, `crates/caco-daemon/src/audio.rs`, `crates/caco-daemon/src/lib.rs`, `docs/transcription.md`, `docs/transcription.html`.
- Tests: added daemon tests for `input_audio` item generation versus text item generation, and CLI metadata coverage for `audio realtime-speech`.
- Behavioural delta: operators and agents have a first-party realtime speech-to-speech command/API path that does not force audio through an STT → text → TTS round trip.

## Operator-takeaway

This lands the initial speech-to-speech wire path and documentation. It is a one-turn realtime API/CLI slice, not a full always-on low-latency microphone/player session; future work can build streaming capture/playback UX on top of this first-party endpoint.
