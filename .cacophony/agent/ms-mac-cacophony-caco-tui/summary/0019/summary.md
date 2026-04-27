# Session summary — Audio tools STT indicator state

## Goal

Continue the TUI improvement loop by closing another parity gap between the speech popup and the Audio tools view: the popup exposed STT indicator-dot visibility, while Audio tools did not.

## Bead(s)

- `bd-3b521e` — TUI Audio tools should show STT indicator dot visibility

## Before state

- Failing tests: none existing for the Audio tools STT indicator-dot row.
- Relevant metrics: not a performance change.
- Context: the Audio tools view describes itself as surfacing settings available from the top-right speech popup, but its STT section showed provider, model, and microphone state only. The popup also had an `STT Indicator Dot` setting that could be `VISIBLE` or `HIDDEN`.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: Audio tools now includes a read-only `STT Dot` row in the STT section, showing `VISIBLE` by default and `HIDDEN` when the TUI speech indicator dot is suppressed.

## Diff summary

- Commits: `4aef5c2d8`
- Files touched: `crates/caco-tui/src/views/audio.rs`
- Tests: +2 regression tests / -0 / flipped 0
- Behavioural delta: no speech-control semantics changed; the Audio tools diagnostic surface now reflects the current STT indicator-dot visibility state.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui audio_view_shows_stt_indicator_dot --lib`

## Operator-takeaway

Audio tools now more accurately mirrors the top-right speech popup, so operators can inspect whether the STT visual-state dot is visible or hidden without opening the popup.
