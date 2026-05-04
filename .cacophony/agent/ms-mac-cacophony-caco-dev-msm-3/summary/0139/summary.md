# Session summary — muted TTS requests as skips

## Goal

Implement `bd-d9a0d0`: intentional TTS mute-policy blocks should not be recorded as daemon exceptions/log_error rows while still leaving callers and traces with a clear muted/skipped result.

## Bead(s)

- `bd-d9a0d0` — `[doctor] muted TTS requests are logged as exceptions`

## Changes

- Updated `crates/caco-cli/src/lib.rs` TTS daemon speech-response handling:
  - Added `tts_daemon_speech_response_is_expected_mute(...)` to classify `409 Conflict` API responses with envelope code `audio_muted` as expected mute-policy skips.
  - For expected mute-policy skips, the TTS daemon now records a trace outcome `skipped` with rule `audio-muted`, logs an INFO line, increments processed count, and acknowledges the message as an `audio-muted skip`.
  - Expected mute-policy skips no longer call `tts_log_and_report_error(...)` and no longer increment daemon failure counters.
  - Non-mute speech API errors continue to be traced as failures, reported as structured errors, counted as failures, and acknowledged as `speech API failure`.
- Updated `SPEC.md` to require configured mute-policy blocks to be expected skips rather than daemon exception feed pollution.
- Updated `README.md` TTS operator helper notes to document that intentional mute-rule suppression is skipped playback, not a daemon exception.

## Validation

- `rustfmt --edition 2021 --check --config skip_children=true crates/caco-cli/src/lib.rs` — passed.
- `git diff --check` — passed.
- `cargo test -p caco-cli tts_daemon_speech_response_classifies_audio_muted_as_skip_bd_d9a0d0 -- --test-threads=1` — passed.
- `cargo clippy -p caco-cli --lib --no-deps -- -D warnings` — passed.

## Notes

- The daemon API still returns the existing clear `audio_muted` envelope/status to callers; this slice changes only the headless TTS daemon's classification/logging so expected policy suppression does not hide real exceptions.
