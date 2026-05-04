# bd-58308e — transcription E2E synthetic speech coverage

## Bead
- bd-58308e — Expand transcription E2E synthetic speech coverage

## Changes
- Added stdin/pipeline support for deterministic STT fixtures:
  - `caco audio transcribe --file -` now reads WAV/audio bytes from stdin.
  - `caco audio doctor --clip -` now reads the self-test clip from stdin and reports the clip as `<stdin>`.
- Extended the hermetic integration E2E test to exercise a true synthetic speech pipe:
  - `caco audio speak --stdout ...` -> `caco audio transcribe --file - --json`.
  - `caco audio speak --stdout ...` -> `caco audio doctor --clip - --model fixture-stt --json`.
  - Asserts the deterministic transcript text appears in CLI JSON and doctor self-test output.
- Updated CLI help metadata, `SPEC.md`, `README.md`, `AGENTS.md`, and transcription docs/HTML to document the pipe-friendly first-party regression paths.

## Validation
- `rustfmt --edition 2021 --check crates/caco/tests/integration_e2e.rs crates/caco-cli/src/audio_cmd.rs crates/caco-cli/src/lib.rs` — passed.
- `git diff --check` — passed.
- `./docs/validate-pages.sh` — passed, 3313 checks.
- `caco test run --wait --command "cargo test -p caco --test integration_e2e synthetic_speech_transcription_round_trip_reaches_cli_and_daemon -- --nocapture"` — passed as job `tj-836ea76c`.
  - Earlier job `tj-7338434e` failed before the parser accepted bare `-` as a value for `--file`; the parser fix is included and the follow-up job passed.

## Notes
- No live provider credentials, microphone, or ambient host TTS CLI are required; the fixture remains command-template based.
- The parser change is intentionally narrow: bare `-` is treated as a flag value only for `caco audio transcribe --file -` and `caco audio doctor --clip -`.
