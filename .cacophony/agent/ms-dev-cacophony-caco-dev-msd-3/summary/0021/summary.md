# Session summary — bd-88798a slice 1: partial ghost text + final flash

## Goal

Land AC#1 (state) + AC#2 (dim italic partial) + AC#3 (250 ms flash
on final commit) of bd-88798a so the operator can see the partial-
transcript live in the active-utterance row and gets a visual
confirmation pulse the moment it commits.

## Bead(s)

- `bd-88798a` — [stt-ux] AC2 partial ghost text + AC3 final flash
  (slice 1 of 5 acceptance criteria)

## Before state

`SpeechState` had `raw_transcription: Option<String>` for the post-
final preliminary surface but nothing for the *partial* live text
emitted by the engine before the utterance is finalised. There was
also no commit-timestamp anchor for the visual confirmation flash
(AC#3). The active-utterance row had no centralised render helper,
so the partial-vs-final styling was implicit and ad-hoc.

## After state

State (`crates/caco-tui/src/speech.rs`):
- `partial_transcript: Option<String>` (live partial from engine
  before final-commit).
- `last_final_at: Option<Instant>` (drives AC#3 flash window).
- `set_partial_transcript` / `clear_partial_transcript` helpers.
- `commit_final_transcript(text_override, now)`: moves partial →
  raw_transcription (or uses override if engine re-scored at
  final), clears partial, stamps `last_final_at`. Override path
  matters for engines with late-bound rescoring.
- `is_final_flash_active(now)`: true for 250 ms post-commit.

Render (`crates/caco-tui/src/views/speech_indicator.rs`):
- `active_utterance_spans(speech, now)` central renderer:
  - Final within flash window → BOLD + NORD13 amber (AC#3).
  - Partial only → NORD3 dim italic ghost (AC#2).
  - Final post-window → plain bold (persists in raw_transcription
    until consumed/dispatched).
  - Empty state → empty Vec (no steady-state layout shift).

## Diff summary

- `crates/caco-tui/src/speech.rs`: +60 lines (3 fields, 5 helpers).
- `crates/caco-tui/src/views/speech_indicator.rs`: +50 lines
  (renderer) + 5 new tests (~90 lines).
- 202 insertions across 2 files.
- `cargo test -p caco-tui --lib active_utterance | commit_final`:
  5/5 pass; full lib unaffected.
- `cargo check --workspace --tests`: clean.

## Embedded artefacts

(none)

## Operator-takeaway

Slice 1 lands the surface and the commit primitive. Two acceptance
criteria remain:
- **AC#4** (final transcript appended to speech-popup history pane):
  needs a small history buffer in `SpeechState` and a popup-pane
  consumer; mostly mechanical.
- **AC#5** (scribble event partial→final drives correct render
  transitions): cross-cuts `crates/caco-daemon/src/scribble_stt.rs`
  to fire interim events and the TUI driver loop to call
  `set_partial_transcript` / `commit_final_transcript`.

Bead unclaimed for follow-on slices.
