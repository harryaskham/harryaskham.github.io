# Session summary — bd-a1049b: voice-attach suppression is now visible in the TUI

## Goal

Fix the operator-facing bug split out of bd-e251c6: when the TUI is
voice-attached to one agent, speech from every other agent was filtered as
working-as-designed — but the operator had almost no visible clue why the
fleet had suddenly gone quiet. The goal was to preserve the focus-mode filter
while making suppression impossible to miss.

## Bead(s)

- `bd-a1049b` — `[tts/tui] Voice-attach to an agent SILENTLY drops all other agents' speeches with no operator-visible affordance`

## Before state

Before this fix:

- `caco-tui` intentionally filtered speech playback to the attached agent when
  `voice_attached_agent` was set.
- The suppression happened at two enqueue points in `state/mod.rs`:
  - `UiEventType::SpeechRequested`
  - feed-event path for `message_speak`
- The filter was silent:
  - no suppression counter
  - no suppression toast
  - no visible clue that non-attached agents were being dropped right now
- There was already a small attach indicator (`🎤↔agent`) in the speech
  indicator, and attach/detach toasts existed, but they were too easy to miss
  and did not explain why later speech vanished.

Observed operator symptom:

- “I can hear the winmini agents but nobody else”

That matched an active voice-attach to a winmini agent, with every other sender
being filtered from local TTS playback.

## After state

The filter still exists — focus mode remains by design — but it is no longer
silent.

What changed:

1. **Suppression counter in `SpeechState`**
   - added `voice_attach_suppressed_count`
   - added `voice_attach_last_suppressed_sender`
   - attach/detach now resets this state so the count is scoped to the current
     attach session

2. **Visible warning toast on first/new suppressed sender**
   - when speech from a non-attached sender is filtered, the TUI now emits a
     warning toast like:
     - `🎤 Voice attached to <agent>; suppressed speech from <sender>. Open that agent and press Ctrl+V to detach.`
   - this avoids silent loss while also avoiding toast spam on every repeated
     drop from the same sender

3. **Persistent indicator count**
   - the speech indicator now shows a visible red suppression count while voice
     attach is active and drops have occurred:
     - `🎤↔agent ⛔3`
   - this makes the condition persist in the chrome instead of existing only as
     a transient toast

4. **Clearer attach affordance**
   - the attach toast now explicitly says `Ctrl+V to detach`
   - this makes the escape hatch discoverable at the moment attach mode begins

## Diff summary

Files touched:

- `crates/caco-tui/src/speech.rs`
- `crates/caco-tui/src/state/mod.rs`
- `crates/caco-tui/src/state/tests.rs`
- `crates/caco-tui/src/views/speech_indicator.rs`
- `crates/caco-tui/src/app.rs`
- `.cacophony/agent/winmini-cacophony-caco-dev-wmi-2/summary/0027/summary.md`

Behavioural delta:

- Non-attached agent speech is still not played during voice-attach focus mode
- But the operator now gets:
  - a toast the first time (or on sender change)
  - a persistent suppression count in the speech indicator
  - a clearer detach hint

## Verification

Targeted tests added/passing:

- `voice_attach_suppressed_speech_increments_count_and_toasts`
- `voice_attach_toggle_resets_suppression_counter`
- `shows_voice_attach_indicator_with_suppressed_count`

Validation run:

- `cargo test -p caco-tui --lib voice_attach_suppressed_speech_increments_count_and_toasts` — pass
- `cargo test -p caco-tui --lib voice_attach_toggle_resets_suppression_counter` — pass
- `cargo test -p caco-tui --lib shows_voice_attach_indicator_with_suppressed_count` — pass
- `cargo build -p caco-tui` — clean
- `cargo test-small` — pass (`187 passed`)

Clippy note:

- `cargo clippy -p caco-tui --all-targets -- -D warnings` was blocked by a
  pre-existing unrelated `caco-daemon` lint (`dispatch_ambient_notification`
  too_many_arguments). No new clippy issue from the TUI changes themselves was
  observed in the targeted test/build slice.

## Operator-takeaway

The root cause was not broken cluster-wide TTS — it was a TUI focus-mode UX
trap. With this change, voice-attach can still intentionally focus on one
agent, but it can no longer make the rest of the fleet go mysteriously silent.
If speech is being suppressed because voice-attach is active, the operator now
gets both an immediate warning and a persistent visible count, plus an explicit
detach hint.