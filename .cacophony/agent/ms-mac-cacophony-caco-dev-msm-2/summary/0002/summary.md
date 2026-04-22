# Session summary — caco audio speak --express-as (bd-5b199b)

## Goal

Land the explicit per-invocation slice of bd-5b199b: a `--express-as` flag (and optional `--styledegree`) on `caco audio speak` that wraps Azure SSML output in `<mstts:express-as style='STYLE' styledegree='X.X'>`. The full config-driven rotation (voice_pattern gating, styledegree sampling, overnight override windows, agent-id-keyed determinism) is deferred — this slice gives operators and other CLIs an immediate knob without locking in the rotation schema.

## Bead(s)

- `bd-5b199b` — [bd-45ea63 follow-up] SSML azure_express_as config block + --express-as flag

## Before state

- `SpeechRequest` had `speed` and `instructions` overrides but no SSML express-as field.
- `build_azure_proxy_ssml(text, voice, speed)` produced fixed SSML with no wrapping hook for `<mstts:express-as>`.
- `caco audio speak` exposed `--speed` and `--instructions` only.
- The schema sketch in `.cacophony/tts.yaml` (TODO comment block) was the only acknowledgement of express-as.

## After state

- `cargo test -p caco-daemon --lib audio::` — 108 passed (7 new express-as tests).
- `cargo test -p caco-cli --lib audio_speak` — 4 passed (2 new express-as tests).
- `cargo test-small` — 197 / 109 / 716 / 277 / 18 / 2786 / 45 passed, 0 failed.
- `cargo clippy -p caco-cli -p caco-daemon --no-deps` — clean (the one stripping-prefix warning is pre-existing in caco-daemon's build script).
- `caco audio speak --help` lists the two new flags.

## Diff summary

- Commit: `272fddf8`
- Files touched:
  - `crates/caco-daemon/src/audio.rs` — `SpeechRequest` gains `express_as: Option<String>` and `styledegree: Option<f32>`. New `build_azure_proxy_ssml_with(text, voice, speed, express_as, styledegree)` does the wrapping for both 1p Mai-Voice-Finetuned-1 (inside `<mstts:turn>`) and 3p Azure neural voices. Original `build_azure_proxy_ssml` preserved as a thin shim. styledegree clamped to 0.01..2.0. `none` (case-insensitive) is the disable sentinel. 7 new tests.
  - `crates/caco-cli/src/lib.rs` — `AUDIO_SPEAK_ARGS` gains `--express-as` and `--styledegree`. `dispatch_audio_speak` forwards both. `build_audio_speech_request_body` grows two args (with `#[allow(clippy::too_many_arguments)]`). 2 new tests; existing test calls updated.
- Tests: +9 unit (7 daemon, 2 CLI); 0 removed; 0 flipped.
- Behavioural delta: opt-in only — no SpeechRequest payload that omits `express_as` changes behaviour. With `express_as: Some("cheerful")` and an Azure voice, the dialog body is wrapped accordingly.

## Operator-takeaway

`caco audio speak --voice en-US-AvaNeural --model azure/speech/azure-tts --text "Hi" --express-as cheerful --styledegree 1.5` now produces SSML with the express-as wrapper. Non-Azure voices silently ignore it (the SSML strip path drops the tag). The full rotation/config block (sampling, overnight windows, voice_pattern gating) is still TODO — file a fresh follow-up bead when that becomes the next priority. The schema sketch in `.cacophony/tts.yaml` was intentionally left untouched so the rotation work can land it under one cohesive commit.
