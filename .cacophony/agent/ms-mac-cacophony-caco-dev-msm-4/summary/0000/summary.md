# Session summary — bd-d79348 voice-call reply TTS seam

## Goal
Unblock the remaining voice-call orchestration criterion by choosing and encoding the concrete TTS playback path for live controller replies, keeping the protocol crate pure and testable.

## Bead(s)

- `bd-d79348` — [stt-ux] Decide and wire TTS reply playback path for voice-call orchestration
- Related parent: `bd-07d590` — [stt-ux] Voice-call orchestration: live operator-to-controller bidirectional flow with explicit handoff signals + transcript log

## Before state

- `bd-07d590` already had direct-DM routing, handoff chimes, transcript rendering, and voice-call mode hints, but its notes said criterion 3 remained blocked on a TTS engine/path decision.
- There was no explicit reply playback envelope connecting live controller replies to an operator-audible TTS path.

## After state

- Chose the existing local TUI speech queue backed by the daemon audio proxy as the canonical playback path for live voice-call replies.
- Added `ReplyPlaybackEngine::TuiSpeechQueue`, `ReplyPlaybackRequest`, and `build_reply_playback_request` in `caco-stt-protocol`.
- The helper suppresses empty or ended-session replies, trims live replies, marks them as voice-call traffic, and preserves transcript/playback correlation via session id and source agent id.

## Diff summary

- Commit: `6662891e3` before rebase/reintegration.
- Files touched: `crates/caco-stt-protocol/src/voice_call_orchestration.rs`.
- Tests: added reply playback routing and suppression coverage; updated full orchestration scenario to assert the TUI speech queue path.
- Validation: `cargo test -p caco-stt-protocol voice_call_orchestration --lib`; `cargo clippy -p caco-stt-protocol --all-targets -- -D warnings`; `cargo check --workspace --tests`.

## Operator-takeaway

The live voice-call protocol now has an explicit, tested TTS reply seam: controller replies are queued for the operator through the existing TUI speech/audio path rather than being broadcast or left as an ambiguous future decision.
