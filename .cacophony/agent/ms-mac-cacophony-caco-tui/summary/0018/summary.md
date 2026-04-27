# Session summary — Audio tools default TTS speed

## Goal

Continue the TUI improvement loop by aligning the Audio tools view with the speech popup for TTS speed visibility and formatting.

## Bead(s)

- `bd-c68b8b` — TUI Audio tools should show default TTS speed

## Before state

- Failing tests: none existing for the Audio tools speed row when no explicit speed was configured.
- Relevant metrics: not a performance change.
- Context: the speech popup always showed a local `TTS Speed` row, using `default` for provider default speed, but the Audio tools page omitted the speed row entirely when `speech.tts_speed` was `None`.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: the Audio tools view now always shows a `Speed` row. It renders `default` when no explicit speed is configured and uses compact one-decimal formatting for explicit speeds such as `1.5x`.

## Diff summary

- Commits: `41622ee65`
- Files touched: `crates/caco-tui/src/views/audio.rs`
- Tests: +2 regression tests / -0 / flipped 0
- Behavioural delta: no audio-control semantics changed; the Audio tools view is more informative and consistent with the speech popup.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui audio_view_ --lib`

## Operator-takeaway

The TUI Audio tools page now reports TTS speed even when the system is using the provider default, closing a small visibility gap introduced by the new local speech-popup speed control.
