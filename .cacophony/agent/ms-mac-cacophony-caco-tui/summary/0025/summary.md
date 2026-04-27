# Session summary — Audio tools Input Mute label

## Goal

Continue the TUI improvement loop by aligning the Audio tools STT mute row label with the clearer `Input Mute` wording used in the speech popup.

## Bead(s)

- `bd-0db77e` — TUI Audio tools should label input mute explicitly

## Before state

- Failing tests: none existing for the Audio tools input-mute label.
- Relevant metrics: not a performance change.
- Context: the speech popup exposed the setting as `Input Mute`, while Audio tools labeled the same state as `Microphone`, making the Audio tools setting less directly comparable to the popup.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: Audio tools now labels the row `Input Mute`, preserving the existing `Active` and `MUTED` value display and icon styling.

## Diff summary

- Commits: `26c78328a`
- Files touched: `crates/caco-tui/src/views/audio.rs`
- Tests: +2 regression tests / -0 / flipped 0
- Behavioural delta: no STT control semantics changed; the Audio tools STT section now uses the same explicit setting label as the speech popup.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui audio_view_shows_input_mute --lib`

## Operator-takeaway

Audio tools now names the STT mute state the same way as the popup, making the two TUI speech surfaces easier to cross-check.
