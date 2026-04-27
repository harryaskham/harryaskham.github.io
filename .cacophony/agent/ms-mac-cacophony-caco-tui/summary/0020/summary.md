# Session summary — Audio tools STT unavailable reason

## Goal

Continue the TUI improvement loop by making the Audio tools diagnostics consistent between TTS and STT provider status rows.

## Bead(s)

- `bd-6da914` — TUI Audio tools STT unavailable status should show reason

## Before state

- Failing tests: none existing for the STT unavailable reason display.
- Relevant metrics: not a performance change.
- Context: the TTS provider row in Audio tools displayed `Unavailable: <reason>` when capabilities included an unavailable reason, but the STT provider row only displayed `Unavailable`, hiding useful diagnostic context.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: the STT provider row now mirrors the TTS provider row by displaying `Unavailable: <reason>` when an unavailable reason is present, falling back to plain `Unavailable` otherwise.

## Diff summary

- Commits: `e7757e3c3`
- Files touched: `crates/caco-tui/src/views/audio.rs`
- Tests: +1 regression test / -0 / flipped 0
- Behavioural delta: no audio-control semantics changed; the Audio tools STT diagnostics now surface unavailable reasons.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui audio_view_shows_stt_unavailable_reason --lib`

## Operator-takeaway

When STT is unavailable, the TUI Audio tools view now gives the same kind of actionable reason text as the TTS row instead of hiding the reason behind a generic unavailable label.
