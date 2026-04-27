# Session summary — Audio tools voice rotation state

## Goal

Continue the TUI improvement loop by making the Audio tools TTS section explicitly show local voice-rotation state, matching the top-right speech popup.

## Bead(s)

- `bd-b8688e` — TUI Audio tools should show voice rotation state explicitly

## Before state

- Failing tests: none existing for Audio tools voice-rotation state rendering.
- Relevant metrics: not a performance change.
- Context: Audio tools only appended `[rotation active]` to the Voice row when rotation was enabled. When rotation was off there was no explicit state, unlike the speech popup's clear `Voice Rotation` ON/OFF setting.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: Audio tools now renders an explicit `Rotation` row with `OFF` or `ON`. When enabled, the row preserves the compatible voice-pool count, such as `2 in pool`.

## Diff summary

- Commits: `92b4bf324`
- Files touched: `crates/caco-tui/src/views/audio.rs`
- Tests: +2 regression tests / -0 / flipped 0
- Behavioural delta: no audio semantics changed; the Audio tools view now makes local voice-rotation state visible in both enabled and disabled states.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui audio_view_shows_voice_rotation --lib`

## Operator-takeaway

Audio tools now tells operators directly whether local TTS voice rotation is on or off, instead of only hinting at it when enabled.
