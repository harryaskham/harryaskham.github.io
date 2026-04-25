# Session summary — bd-88798a STT final-commit chime marker

## Goal
Finish the remaining bounded part of the STT partial-to-final UX bead by adding a testable final-commit chime marker while preserving the already-landed partial transcript, flash, and history behavior.

## Bead(s)

- `bd-88798a` — [stt-ux] AC2: live partial-transcript ghost text + AC3 final-commit flash

## Before state

- Prior slices had already landed `partial_transcript`, `last_final_at`, active utterance rendering, final flash timing, transcript history, and partial-to-final render tests.
- The bead was reopened because AC3 still lacked a chime-related marker and the validation path was briefly blocked by broken-on-main `bd-028f7e`, which another worker fixed.

## After state

- Added `SpeechState::pending_final_chimes` as a drainable count of final transcript commits that should trigger the optional confirmation chime.
- `commit_final_transcript` increments the marker with saturating arithmetic whenever a final boundary is committed.
- Added `drain_pending_final_chimes()` so the UI/audio integration can clear the marker exactly once after playback.
- Added regression coverage that final commits enqueue and drain the chime marker.

## Diff summary

- Commits: `129575e83` before rebase, replayed on current main.
- Files touched: `crates/caco-tui/src/speech.rs`, `crates/caco-tui/src/views/speech_indicator.rs`.
- Tests: added `commit_final_transcript_enqueues_drainable_chime_marker`.
- Validation: `cargo test -p caco-tui commit_final_transcript_enqueues_drainable_chime_marker --lib`; `cargo test -p caco-tui speech_indicator_partial_then_final_transitions --lib`; `cargo clippy -p caco-tui --all-targets -- -D warnings`; `cargo check --workspace --tests`.

## Operator-takeaway

The STT final-commit path now has a concrete, test-covered hook for a soft confirmation chime, completing the reopened non-visual part without disturbing the existing partial/final transcript UX.
