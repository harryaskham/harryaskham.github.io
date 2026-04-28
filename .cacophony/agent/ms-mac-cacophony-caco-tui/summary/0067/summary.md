# Session summary — Speech indicator uses active theme colors

## Goal

Continue the TUI theme-hardcode sweep by converting the compact speech/STT/TTS indicator from fixed Nord colors to active-theme semantic colors.

## Bead(s)

- `bd-74a067` — Speech indicator should use active TUI theme colors

## Before state

- Failing tests: none; this was a source-inspection theme consistency issue.
- Relevant metrics: not benchmarked.
- Context: `crates/caco-tui/src/views/speech_indicator.rs` hardcoded Nord colors for unavailable/mute icons, STT state dots, recording/transcribing states, input level colors, target labels, voice attach indicators, daemon mute icons, TTS icons, STT error toasts, and active utterance highlights.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: speech indicator colors now resolve through `common::theme()` semantic accessors while preserving the same state-to-color meanings: red for unavailable/error, dim for muted/idle, green for active, yellow for warning/transcribing, purple for speech playback, and accent/frost for links/targets.

## Diff summary

- Commits: `15804bb2b`
- Files touched: `crates/caco-tui/src/views/speech_indicator.rs`
- Tests: focused speech indicator tests passed
- Behavioural delta: no speech/STT/TTS state or icon changes; visible colors now follow the active TUI theme.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui speech_indicator --lib`

## Operator-takeaway

The always-visible speech controls in the header now respect enterprise/custom palettes instead of leaking Nord status colors into the top bar.
