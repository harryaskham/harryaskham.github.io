# Session summary — wire STT partial/final transcript into TUI

## Goal

Take the dormant `partial_transcript` / `commit_final_transcript` / `transcript_history` plumbing in `caco-tui` (already implemented and tested in isolation) and actually surface it on the screen so AC#2 ghost text, AC#3 final-commit flash, and AC#4 popup history light up in real use.

## Bead(s)

- `bd-88798a` — [stt-ux] AC2: live partial-transcript ghost text + AC3 final-commit flash
- (parent: `bd-c16753` — [stt-ux] Visual STT indicators in TUI; umbrella)

## Before state

- Failing tests: none related (broken-on-main `config_distribute_with_distribute_command_node_uses_command` taken by po4-5)
- State helpers `set_partial_transcript`, `commit_final_transcript`, `is_final_flash_active`, `active_utterance_spans`, `transcript_history_lines` all existed and unit-tested.
- Zero call-sites of `commit_final_transcript`, `active_utterance_spans`, or `transcript_history_lines` outside their own test module — all dead from the operator's POV.
- `ActionResult::TranscriptionCompleted` only called `set_raw_transcription` + `finish_transcription`, so `last_final_at` was never stamped, so AC#3 flash never fired and AC#4 history was never appended.

## After state

- Failing tests: none in `caco-tui --lib` (2909 passed).
- `TranscriptionCompleted` now also calls `commit_final_transcript(Some(text), Instant::now())`.
- Tab-bar speech indicator inlines `active_utterance_spans` when a partial is in flight or we are in the 250ms flash window; otherwise skips so it doesn't duplicate the existing raw_transcription block.
- Speech popup grew a "Recent transcripts" footer rendering up to 5 history lines from `transcript_history_lines`; popup_rect updated for hit-testing.
- 3 new tests cover the wiring against the real `speech_indicator_spans` output (partial visible, final visible during flash, partial→final transition).

## Diff summary

- Commit: `f57cf68a4`
- Files touched: `crates/caco-tui/src/app.rs`, `crates/caco-tui/src/views/speech_indicator.rs`, `crates/caco-tui/src/views/speech_popup.rs`
- Tests: +3
- Behavioural delta: operators now actually see the partial ghost text, the post-commit flash, and a scrollback of recent finalised utterances inside the speech popup.

## Operator-takeaway

bd-88798a is a wiring-only bead: prior agents had built and unit-tested the entire surface but never called it from the render path or the event-handler. Always grep for call-sites of newly-added public helpers before declaring acceptance — passing unit tests on a function nobody invokes is the easiest way to ship dead UI code. The streaming partial path (engine → `set_partial_transcript`) still has no producer; partials will only appear once a streaming STT engine is wired to push interim events. AC#3's "soft chime" was deferred — TTS emit on commit is one line away but no chime asset exists in tree yet; left for a follow-up.
