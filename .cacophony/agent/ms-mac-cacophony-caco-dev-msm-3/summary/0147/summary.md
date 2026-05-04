# Session summary — bd-da1220 TTS speech policy identity

## Goal

Fix `bd-da1220`: headless TTS daemon synthesis requests were authenticated with the node token and omitted `x-caco-caller`, causing the daemon auth middleware to synthesize `node:_:node-token`. Audio mute policy and `audio_muted` diagnostics therefore named the literal placeholder `node-token` instead of the real source agent.

## Changes

- Updated the headless TTS daemon path in `crates/caco-cli/src/lib.rs` so each `/api/v1/audio/speech` synthesis request includes an explicit `x-caco-caller` derived from the originating feed event.
- Added `tts_daemon_audio_caller(...)` and `clean_tts_caller_part(...)`:
  - preserves canonical feed senders such as `ms-mac:cacophony:real-agent`;
  - canonicalizes short senders with the event project and local node;
  - falls back to `tts-daemon` identities instead of allowing the node-token authentication placeholder to become the user-facing agent id.
- Kept TTS trace sender/project fields aligned with existing feed metadata.
- For `choice_presented` events, uses the presenting `agent_id` as sender when the feed wrapper lacks an explicit sender.
- Updated `SPEC.md` to require the headless TTS daemon to forward the real feed sender identity and keep `node-token` out of agent-authored speech diagnostics.
- Updated `README.md` TTS mute-policy notes with the same operator-facing identity contract.

## Validation

- `rustfmt --edition 2021 --check --config skip_children=true crates/caco-cli/src/lib.rs` — passed.
- `git diff --check` — passed.
- `cargo test -p caco-cli bd_da1220 -- --test-threads=1` — passed.
- `cargo clippy -p caco-cli --lib --no-deps -- -D warnings` — passed.

## Notes

- I did not live-toggle node mute policy or force production TTS playback; validation is source/unit-test based.
- The fix preserves node-token authentication for the TTS daemon but separates authentication class from the speech-policy caller identity used by `/api/v1/audio/speech`.
