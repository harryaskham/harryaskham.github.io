# Session summary — Termux TTS profile for sgu24

## Goal

Add a selectable local Android/Termux TTS option for the `sgu24` mobile node so Cacophony speech can be routed through the existing `tx` Termux execution wrapper and Android's `termux-tts-speak` command without changing the desktop default TTS configuration.

## Bead(s)

- `bd-a8bd72` — Add termux-tts variant and configure as sgu24 local TTS option

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: `sgu24` inherited the shared TTS profile catalog, but no `termux-tts` command-template profile existed in the checked-in TTS effect/profile configuration.
- Context: The bead required a selectable profile that uses the existing `tx` Termux-exec alias, quotes the speech body correctly, and is visible in `sgu24` effective configuration.

## After state

- Failing tests: none observed for this bead.
- Relevant metrics: focused queued validation `tj-80891d2d` passed for `cargo test -p caco-config checked_in_config_exposes_termux_tts_profile_for_sgu24_bd_a8bd72 --lib`.
- Context: `termux-tts` is now registered in `.cacophony/tts.yaml`, documented in `.cacophony/tts/effects.yaml`, implemented as `.cacophony/tts/effects/termux-tts.yaml`, and pinned by a checked-in config regression test against `sgu24` effective speech config.

## Diff summary

- Commits: `9b6795560d`
- Files touched: `.cacophony/tts.yaml`, `.cacophony/tts/effects.yaml`, `.cacophony/tts/effects/termux-tts.yaml`, `crates/caco-config/src/lib.rs`
- Tests: +1 focused config regression test; no tests removed or flipped.
- Behavioural delta: Operators can select `termux-tts` as a command-template TTS profile. The template calls `tx termux-tts-speak "$BODY"` and writes a minimal WAV to `OUTPUT_FILE` so the existing Cacophony command-template acknowledgement/playback pipeline remains satisfied.

## Operator-takeaway

`sgu24` now has a first-party selectable local mobile TTS backend wired through the checked-in configuration, with regression coverage ensuring the profile stays visible and uses the expected quoted Termux command shape.
