# Session summary — bd-b78d9c: add gpt-audio-1.5 to TTS model surface

## Goal

Expose `gpt-audio-1.5` as a first-class TTS model in the existing audio stack so
it appears in supported-model lists, gets a friendly display name, and follows
the same standard-voice compatibility rules as the existing OpenAI TTS model.

## Bead(s)

- `bd-b78d9c` — Add `gpt-audio-1.5` for audio generation capabilities

## Before state

- The daemon's supported TTS model list did not include `gpt-audio-1.5`.
- Voice compatibility logic only treated `gpt-4o-mini-tts` as an OpenAI
  standard-voice model.
- TTS help text examples only mentioned `gpt-4o-mini-tts` and Gemini.

## After state

- `gpt-audio-1.5` is now included in `SUPPORTED_TTS_MODELS`.
- It has a human-friendly display label: `GPT Audio 1.5`.
- Standard OpenAI voices are accepted for both `gpt-4o-mini-tts` and
  `gpt-audio-1.5`.
- Azure finetuned voices remain rejected for both OpenAI-family models.
- CLI TTS help text examples now mention `gpt-audio-1.5` as a valid model.

## Diff summary

- Files touched:
  - `crates/caco-daemon/src/audio.rs`
  - `crates/caco-cli/src/lib.rs`
- Tests:
  - `cargo test -p caco-daemon supported_tts_models_includes_required -- --nocapture`
  - `cargo test -p caco-daemon tts_model_display_names_cover_supported -- --nocapture`
  - `cargo test -p caco-daemon standard_voices_compatible_with_openai_models -- --nocapture`
  - `cargo test -p caco-daemon finetuned_voices_incompatible_with_openai_models -- --nocapture`
  - `cargo test -p caco-daemon voices_for_model_filters_correctly -- --nocapture`
- Behavioural delta:
  - `gpt-audio-1.5` now participates in the normal TTS model enumeration and
    voice-compatibility flow instead of being invisible to the stack.

## Operator-takeaway

This was a contained model-surface wiring task, not a new audio architecture
project. The stack already knew how to handle provider-backed TTS models; it
just needed `gpt-audio-1.5` added to the same supported-model and voice-policy
paths so operators can actually select it.
