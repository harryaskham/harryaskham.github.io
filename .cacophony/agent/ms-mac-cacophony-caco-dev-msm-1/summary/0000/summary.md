# Session summary — caco audio prewarm broken default + capabilities devices

## Goal

Fix the test-user-reported regression where bare `caco audio prewarm` always
errors because its default model is not in the daemon's supported STT model
set, and surface the daemon's mic/speaker devices in the human-readable
`caco audio capabilities` output (currently only available via `--json`).

## Bead(s)

- `bd-06075c` — caco audio prewarm default --model 'large-v3-turbo-q8_0' is
  not in the supported STT model list — bare invocation always errors;
  help example uses broken default

## Before state

- `caco audio prewarm` (no args) → `error: Failed to prewarm model
  'large-v3-turbo-q8_0': Unsupported STT model 'large-v3-turbo-q8_0'.
  Supported: gpt-4o-mini-transcribe, whisper, scribble`.
- `caco audio prewarm --help` example was the same broken default.
- `caco audio capabilities` text output omitted `input_devices` and
  `output_devices` entirely; only `--json` exposed them.
- Failing tests: none for this code (manual repro only).

## After state

- `dispatch_audio_prewarm` defaults to `"whisper"` — a model in
  `SUPPORTED_STT_MODELS` (`gpt-4o-mini-transcribe, whisper, scribble`).
- CLI flag help example updated to `whisper`.
- `dispatch_audio_capabilities` text formatter renders Input devices and
  Output devices with `[default]` markers, taken from the existing
  `input_devices` / `output_devices` JSON arrays.
- Build clean (`cargo build -p caco-cli`), clippy clean
  (`cargo clippy -p caco-cli --lib`), no test additions (pure CLI surface).

## Diff summary

- Commit: 9305224d
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: +0 / -0 / flipped 0
- Behavioural delta: bare `caco audio prewarm` now succeeds against an
  API-based default; `caco audio capabilities` text output now lists
  the daemon's audio devices.

## Operator-takeaway

The `audio capabilities` JSON envelope is the source of truth for device
enumeration; the text formatter just needed to be taught the shape. The
prewarm default was never valid — the original `large-v3-turbo-q8_0`
string is a whisper.cpp model file name, not a daemon STT identifier, so
the help example shipped a copy-pasteable failure. If we later want to
prewarm specific whisper variants (q4, q5, q8), the right place is the
daemon-side validator, not the CLI default.

Out-of-scope but noted in the bead: voices are listed flat without
per-model grouping (28 voices across 3 providers). Worth a follow-up
bead if the operator cares about it.
