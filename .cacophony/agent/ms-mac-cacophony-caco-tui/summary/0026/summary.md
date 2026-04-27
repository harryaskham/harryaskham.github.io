# Session summary — Audio tools STT Indicator Dot label

## Goal

Continue the TUI improvement loop by aligning the Audio tools STT indicator-dot row label with the full wording used in the speech popup.

## Bead(s)

- `bd-0db824` — TUI Audio tools should use full STT Indicator Dot label

## Before state

- Failing tests: none existing for the exact STT indicator-dot label text.
- Relevant metrics: not a performance change.
- Context: Audio tools used the abbreviated label `STT Dot`, while the speech popup used the clearer `STT Indicator Dot` label for the same setting.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: Audio tools now labels the row `STT Indicator Dot` and still shows `VISIBLE` or `HIDDEN` state.

## Diff summary

- Commits: `40cc0a111`
- Files touched: `crates/caco-tui/src/views/audio.rs`
- Tests: updated 2 regression tests / -0 / flipped 0
- Behavioural delta: no STT semantics changed; the Audio tools label now matches the speech popup.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui audio_view_shows_stt_indicator_dot --lib`

## Operator-takeaway

The Audio tools STT indicator-dot row now uses the same full label as the speech popup, removing another small mismatch between the two TUI speech surfaces.
