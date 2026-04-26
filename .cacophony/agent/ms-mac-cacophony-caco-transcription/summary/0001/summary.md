# Session summary — configured live STT PulseAudio routing

## Goal

Make Cacophony live transcription honor the same configured audio input routing as the TUI, especially named PulseAudio sources such as `sgu24` / `source.default`, while keeping operator documentation centered on the first-party `caco audio` path rather than standalone STT helpers.

## Bead(s)

- `bd-b1faef` — Honor named PulseAudio input source devices for STT capture
- Follow-up filed: `bd-929911` — Expose first-party live transcription session control through caco-audio MCP/plugin

## Before state

- Failing tests: none known for this bead; the gap was behavioural/routing coverage.
- Relevant metrics: targeted live routing tests for this behaviour did not exist.
- Context: live PulseAudio capture from `sgu24` worked when manually captured as mono 16 kHz WAV and passed to `caco audio transcribe --file`, but named PulseAudio source devices were not consistently propagated into TUI/CLI live capture commands. `caco audio transcribe --live` also did not document or expose route overrides and did not read stdin.

## After state

- Failing tests: none in completed validation.
- Relevant metrics: `cargo test-small` passed, including 2959 `caco-tui` unit tests and 268 `caco-web` unit tests in the visible tail; targeted caco-cli live routing tests passed 4/4; docs QA passed 1463/1463.
- Context: named PulseAudio input/output routes now carry configured source/sink device names into `parec` / `paplay`; `caco audio transcribe --live` resolves `speech.stt.input` / `speech.io.input` and supports `--input` / `--device` one-shot overrides.

## Diff summary

- Commits: `57c8c64b0` (implementation), plus recorded-summary commits
- Files touched: `.cacophony/config.yaml`, `SPEC.md`, `crates/caco-cli/src/audio_cmd.rs`, `crates/caco-cli/src/lib.rs`, `crates/caco-tui/src/playback.rs`, `crates/caco-tui/src/recording.rs`, `crates/caco-tui/src/speech.rs`, `docs/transcription.md`, `docs/transcription.html`
- Tests: added CLI live routing tests plus TUI PulseAudio source/sink routing tests.
- Behavioural delta: `sgu24` now declares `device: "source.default"`; TUI recording/playback and CLI live transcription can pass explicit PulseAudio source/sink devices instead of relying on the remote server default. Docs now state that `--live` does not read stdin and that direct PulseAudio smoke tests should wrap mono 16 kHz capture as WAV before using `caco audio transcribe --file`.

## Operator-takeaway

The important fix is that live transcription is now configuration-driven: the first-party `caco audio` and TUI paths can target the actual remote microphone source (`source.default`) instead of silently recording the PulseAudio server default or a monitor source. A separate follow-up bead tracks the larger `/stt` and MCP/plugin live-session UX.
