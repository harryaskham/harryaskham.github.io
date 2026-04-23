# Session summary — bd-abd7ba 'Test STT' diagnostic foundations

## Goal

Land the deterministic, testable parts of the 'Test STT' button (built-in
synthetic clip corpus, canonical WER scoring, result value type with a
popup-renderable summary, rotation helper) so subsequent UI-wiring beads
can compose on a stable foundation without re-inventing the metric.

## Bead(s)

- `bd-abd7ba` — [stt-ux] 'Test mic' button in TUI speech-popup: plays known synthetic clip → runs through STT → shows transcript + WER vs ground-truth

## Before state

- No 'Test STT' affordance anywhere; operator must hand-roll an end-to-end voice loop to validate the STT pipeline.
- No canonical WER implementation in the workspace, so any future accuracy-tracking bead (bd-72fd72 permanent) would have to invent one.
- No synthetic corpus: bench beads (bd-68b76d) had no shared fixtures to start from.
- Failing tests: bd-c19193 (pre-existing, unrelated).

## After state

- New `crates/caco-tui/src/views/stt_diagnostic.rs` (mod-registered in `views/mod.rs`):
  - `SyntheticClip { name, ground_truth, pcm_16k_mono }` and `BUILTIN_CLIPS` array of 3 clips covering greeting / multi-word command / agent-id-style token (the hardest case, also relevant to bd-c1930c grammar bias). PCM payloads are 100ms zero buffers today; a follow-up swaps in real TTS-synthesised audio without changing the public API.
  - `compute_wer(reference, hypothesis) -> f32` implementing canonical word-error-rate over whitespace-tokenised, case-insensitive, punctuation-stripped tokens using two-row Levenshtein. Matches the Kaldi / ESPnet convention so the metric is engine-comparable.
  - `TestSttResult` value type with `success(clip, transcript, latency_ms)` and `failure(clip, error_class, detail)` constructors.
  - `summarize_for_popup(result) -> String` renderer producing the operator one-liner: `greeting-1: ✓ WER=0% lat=87ms — hello fleet → hello fleet` for successes, `greeting-1: ! audio_io: no input device` for failures. Status icons: ✓ <1%, • <25%, ✗ ≥25%.
  - `next_clip_index(prev) -> usize` for the rotating-display affordance.
- 15 unit tests covering corpus invariants, WER core (perfect / case+punct / full mismatch / partial substitution / insertions / deletions / empty edge cases), TestSttResult shape, popup summary rendering (success + error paths), rotation completeness + wrap-around, and the bead's acceptance criterion 5 ("programmatic invocation returns deterministic result on a fixture clip") as a dedicated test.
- Failing tests: bd-c19193 (unchanged, pre-existing).

## Diff summary

- Commit: `c9e5b973 bd-abd7ba: 'Test STT' diagnostic foundations — synthetic corpus + WER scoring + deterministic test fixture`
- Files touched:
  - `crates/caco-tui/src/views/stt_diagnostic.rs` (new, ~360 lines including tests)
  - `crates/caco-tui/src/views/mod.rs` (+2 lines, mod registration)
- Tests: +15 / -0 / flipped 0
- Behavioural delta: a new self-contained module is available; no existing TUI surface changes today.

## Operator-takeaway

Acceptance criterion 5 (programmatic deterministic fixture result) is
met. Criteria 1–4 (button click, audio playback, transcript display,
error UX) are scoped out as follow-ups because they cross subsystems
that would inflate this bead substantially:

- Adding a `last_stt_test_result: Option<TestSttResult>` to
  `SpeechState` and rendering it as a SettingRow in
  `views/speech_popup.rs` is a small change but touches every
  initializer of `SpeechState` across tests; deserves its own bead.
- Audio playback (cpal/rodio) for the synthetic clips and the actual
  scribble STT call wiring need their own scope.

The compute_wer + corpus + TestSttResult primitives are now
available for both bd-68b76d (corpus + WER harness) and bd-72fd72
(accuracy permanent) to build on without forking the metric.
