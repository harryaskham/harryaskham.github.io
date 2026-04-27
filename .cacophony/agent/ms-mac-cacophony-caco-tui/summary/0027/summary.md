# Session summary — Audio tools live TTS daemon details

## Goal

Continue the TUI improvement loop by making the Audio tools TTS Daemon section show the live daemon details already available in the speech popup daemon tab.

## Bead(s)

- `bd-198f64` — TUI Audio tools should show live TTS daemon details

## Before state

- Failing tests: none existing for Audio tools live TTS daemon detail rendering.
- Relevant metrics: not a performance change.
- Context: Audio tools had a TTS Daemon section, but it only showed status, feed delegation, sync, and control port. The speech popup daemon tab exposed richer live state such as mute, voice, model, speed, filter, output, and queue depth.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: when the live daemon status is reachable, Audio tools now renders daemon mute, voice, model, speed, voice filter, output routing, and queue depth. Existing status, feed TTS, sync, and control rows are preserved.

## Diff summary

- Commits: `cd63284df`
- Files touched: `crates/caco-tui/src/views/audio.rs`
- Tests: +1 regression test / -0 / flipped 0
- Behavioural delta: no daemon-control semantics changed; Audio tools now has richer read-only TTS daemon diagnostics.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui audio_view_shows_live_tts_daemon_details --lib`

## Operator-takeaway

Audio tools now exposes the useful live TTS daemon state at a glance, so operators do not need to open the speech popup daemon tab just to see voice/model/speed/filter/output/queue details.
