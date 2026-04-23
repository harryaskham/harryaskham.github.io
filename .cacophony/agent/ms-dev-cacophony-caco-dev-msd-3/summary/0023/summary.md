# Session summary — bd-88798a slice 2: transcript history ring + formatter

## Goal

Land the data primitive for AC#4 (final transcripts appended to a
speech-popup history pane) of bd-88798a so a future designer-touching
slice can wire the popup render with no further state changes.

## Bead(s)

- `bd-88798a` — [stt-ux] AC4 popup history (slice 2 of remaining
  acceptance criteria; AC#1+2+3 landed in slice 1)

## Before state

`commit_final_transcript` (landed in slice 1) stamped `last_final_at`
and moved the partial → `raw_transcription` but did not preserve any
history. Each commit overwrote the previous one. The popup had no
data structure to render from for an AC#4 history pane.

## After state

State (`crates/caco-tui/src/speech.rs`):
- `TranscriptHistoryEntry { final_text, last_partial, committed_at }`
  preserves BOTH the final text and a snapshot of the partial seen
  just before commit. The pre-commit partial enables retrospective
  accuracy diagnosis: a row where final diverges from last_partial
  is a re-scoring event the operator may want to flag.
- `TRANSCRIPT_HISTORY_CAP = 256` (~5 min dense voice work, well
  under any per-frame render-cost ceiling).
- `SpeechState.transcript_history: VecDeque<TranscriptHistoryEntry>`.
- `commit_final_transcript` pushes FIFO with eviction past the cap;
  empty commits (no partial AND no override) are skipped so spurious
  engine ticks don't pollute the pane.

Render (`crates/caco-tui/src/views/speech_indicator.rs`):
- `transcript_history_lines(speech, limit)` returns `Vec<String>`:
  - Newest-first ordering (rev() over the deque).
  - Each row prefixed by an age label (`12s ago`, `3m45s ago`,
    `2h15m ago`) matching the rest of the TUI's render-age idiom.
  - Plain strings (not Spans) so non-ratatui surfaces (web, MCP
    dump, JSON export) can share the formatter.

## Diff summary

- `crates/caco-tui/src/speech.rs`: +TranscriptHistoryEntry +cap
  +field +push-and-evict logic in commit_final_transcript.
- `crates/caco-tui/src/views/speech_indicator.rs`: +formatter +5
  tests.
- 193 insertions / 2 deletions across 2 files.
- `cargo test -p caco-tui --lib transcript_history|commit_final`:
  6/6 pass.
- `cargo check --workspace --tests`: clean.

## Embedded artefacts

(none)

## Operator-takeaway

Slice 2 stops at the data layer because the popup-pane render
question is a UX call (3rd tab? below-settings panel? side-by-side?
auto-collapse-when-empty?). Designers should pick one; once chosen
the wiring is `let lines = transcript_history_lines(speech, area.height);`
followed by a `Paragraph::new(...)` in whatever Rect the layout
allots. Mechanical.

Remaining for bd-88798a:
- **Slice 3** (UX choice + render wiring in `speech_popup.rs`):
  consumer of this commit's formatter.
- **Slice 4** (AC#5: scribble engine event wiring): cross-cuts
  `scribble_stt.rs` to surface interim events and the TUI driver
  loop to call `set_partial_transcript` /
  `commit_final_transcript`. Same daemon-stream-protocol question
  blocks bd-7a8bc1 AC#4 (error toast wiring), so a single bead-
  slice could reasonably land both.

Bead unclaimed for follow-on slices.
