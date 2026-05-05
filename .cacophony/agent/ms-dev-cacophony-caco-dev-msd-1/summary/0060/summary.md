# bd-95222d — Add synthetic live transcription scratchpad E2E coverage

## Summary

Expanded the hermetic transcription E2E coverage so deterministic synthetic speech now proves transcript text reaches additional live/scratchpad/agent-buffer surfaces beyond the existing file/doctor/API paths.

## Changes

- Added a test-only live transcription capture seam (`CACO_TEST_LIVE_TRANSCRIBE_CAPTURE_FILE`) so `caco audio transcribe --live` can be exercised hermetically without a live microphone, PulseAudio, `parec`, provider credentials, or manual interaction.
- Kept ordinary live transcription behavior on the configured PulseAudio capture route.
- Connected foreground live agent-buffer sessions to the active agent scratchpad target when `--agent-buffer` and `--append-to-scratchpad-id` are used, matching the non-blocking `audio live start` agent-buffer behavior.
- Hardened the integration STT fixture so it can recover deterministic synthetic transcript markers embedded inside WAV/live raw fixture bytes.
- Extended `synthetic_speech_transcription_round_trip_reaches_cli_and_daemon` to assert transcript text appears in:
  - `caco audio transcribe --file ... --append-to-scratchpad-id` scratchpad content,
  - `caco audio transcribe --live --agent-buffer` segment/summary output,
  - live scratchpad `[partial transcription] ...` and `[user transcription] ...` markers,
  - scratchpad list filtering for the active agent connection,
  - and the already-covered CLI JSON/text, stdin pipe, doctor, direct daemon endpoint, and UI snapshot speech-history paths.
- Updated SPEC 24.4 to include deterministic live transcription, scratchpad append, and agent-buffer marker/connection assertions in the hermetic localhost lane contract.

## Validation

- `rustfmt --edition 2021 --check crates/caco/tests/integration_e2e.rs crates/caco-cli/src/audio_cmd.rs`
- `git diff --check`
- `caco config validate --config tests/fixtures/config.integration.yml`
- Queued focused integration test: `tj-3cf485c8`
  - `cargo test -p caco --test integration_e2e synthetic_speech_transcription_round_trip_reaches_cli_and_daemon -- --nocapture`
  - Passed: 1 test passed, 0 failed.

## SPEC

- Covers SPEC §15.4 transcription endpoint/history requirements and SPEC §24.4 hermetic localhost transcription fixture requirements.
