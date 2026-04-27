# Session summary — Audio tools Read Messages Aloud label

## Goal

Continue the TUI improvement loop by aligning the Audio tools TTS read-aloud setting label with the clearer wording used in the speech popup.

## Bead(s)

- `bd-540647` — TUI Audio tools should use full Read Messages Aloud label

## Before state

- Failing tests: none existing for the Audio tools read-aloud label.
- Relevant metrics: not a performance change.
- Context: Audio tools abbreviated the setting as `Read Msgs`, while the top-right speech popup called the same setting `Read Messages Aloud`.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: Audio tools now uses the full `Read Messages Aloud` label and keeps the ON/OFF state visible.

## Diff summary

- Commits: `0865ef6f4`
- Files touched: `crates/caco-tui/src/views/audio.rs`
- Tests: +2 regression tests / -0 / flipped 0
- Behavioural delta: no speech-control semantics changed; the Audio tools label is clearer and consistent with the speech popup.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui audio_view_ --lib`

## Operator-takeaway

The Audio tools TTS section now uses the same clear `Read Messages Aloud` wording as the speech popup, reducing one more small mismatch between the two TUI speech surfaces.
