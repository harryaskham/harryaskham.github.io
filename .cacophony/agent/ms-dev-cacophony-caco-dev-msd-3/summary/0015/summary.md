# Session summary — bd-c16753 slice 1: visual STT dot

## Goal

Operator stated frame: "clear visual indicators for when speech is
correctly detected." Ship acceptance criterion #1 of bd-c16753 —
the persistent at-a-glance STT dot in the tab-bar speech block.

## Bead(s)

- `bd-c16753` — [stt-ux] Visual STT indicators in TUI: live VAD dot,
  partial-transcript ghost text, final-confirmed flash, error toast
  (slice 1 / 6 acceptance criteria; bead remains in_progress)

## Before state

- speech_indicator only rendered a fine-grained dB-meter / mic-icon
  detail block (bd-bcf470, bd-56e4b1) which required the operator
  to read a bar chart to know whether their voice was being
  captured.
- No distinction between "mic open, hearing nothing" and "mic
  open, hearing you" — both showed the unmuted-mic icon with a
  level bar that just happened to be dim in the silent case.
- No persistent at-a-glance state indicator.

## After state

- New `SttVisualState` enum in speech.rs with 4 distilled states:
  `Off / Listening / Hearing / Transcribing` mapped to red / grey /
  green / amber dots.
- New `SpeechState::stt_visual_state()` method derives the state
  from existing `recording / input_muted / capabilities` fields
  plus the new sustained-silence detector.
- New `silent_recording_since: Option<Instant>` field +
  `is_silent_recording_warning()` helper. Armed by
  `update_input_level` when a sample falls below -45 dB; disarmed
  when a louder sample arrives. Returns true once the timer has
  been continuously armed for ≥ 2s.
- Lifecycle hooks: `start_recording` pre-arms the timer (dot
  starts grey until first audible sample); stop / cancel / finish
  all clear it.
- `speech_indicator_spans()` prepends a single colored dot
  (NORD11 / NORD3 / NORD14 / NORD13) before the existing detail
  block. Detail block unchanged — the dot is purely additive.
- Operator now sees: red dot = STT off, grey dot = listening but
  hearing nothing (e.g. muted device, walked away), green dot =
  hearing you, amber dot = engine working on the transcript.

## Diff summary

- `crates/caco-tui/src/speech.rs` (+105):
  - `SttVisualState` enum + `icon()` / `label()` helpers.
  - `silent_recording_since` field + reset hooks in start_recording
    / stop_recording / cancel_recording / finish_transcription.
  - `update_input_level` arms/disarms the silence timer.
  - `is_silent_recording_warning()` + `stt_visual_state()` methods.
- `crates/caco-tui/src/views/speech_indicator.rs` (+182 incl. tests):
  - Dot prepended to spans in front of the detail block.
  - 7 new tests covering all dot states + the silence-timer reset.
- All 45 `views::speech_indicator::tests::*` pass.
- `cargo clippy -p caco-tui` shows 4 pre-existing warnings, none
  in the diff.

## Embedded artefacts

(none)

## Operator-takeaway

The "is the engine actually hearing me?" question now has a
1-glyph answer. Slice scope intentionally tight so the bead can
keep being burned down; remaining acceptance items (partial-text
ghost, final-flash + chime, error toast, settings toggle, scripted
event tests) each map to natural follow-on slices that other
agents (or the next cycle of msd-3) can pick up without
co-ordinating with this commit. Bead is left in_progress;
unclaiming so the next free agent can grab it.
