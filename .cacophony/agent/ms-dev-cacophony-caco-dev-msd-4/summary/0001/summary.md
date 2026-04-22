# Session summary — bd-8c9869 stale-TTS skip on the ingress paths

## Goal

Stop the TTS daemon and the TUI from speaking weeks-old messages
aloud during SSE reconnect, full-state hydration, cross-node sync,
and feed backfill. Add a configurable age threshold so that on
operator-visible audio surfaces the past stays in the past.

## Bead(s)

- `bd-8c9869` — TTS speaks stale speak-messages received over the
  wire — operator heard weeks-old messages on sgu24 during feed
  replay.
- `bd-fabf46` (filed) — pre-existing caco-cli `tests::agent_logs_*`
  failures, unrelated.

## Before state

- Failing tests: none caused by this bead.
- Behaviour: every `message_speak` / `speech_requested` event the
  TTS daemon SSE loop or the TUI's MessageSpeak / feed-event
  enqueue paths saw was unconditionally played, regardless of the
  event's authored timestamp. Operator confirmed that on sgu24 a
  feed replay produced audible playback of messages from days
  prior.
- No config knob existed to bound TTS age.

## After state

- Failing tests caused by this change: none.
- New behaviour: both ingress paths consult
  `tts.max_age_before_skip_secs` (default `300`, set `0` to
  disable). When the event's authored timestamp is older than the
  threshold the audible playback is suppressed; the message
  remains stored, hydrated into chat surfaces, and visible via
  audio-cache replay. The TTS daemon still sends an ack on the
  skip path so `caco msg speak --wait` callers never hang.
- New tests: `cargo test -p caco-cli --lib extract_speakable_authored`
  4 passed; `cargo test -p caco-tui --lib tts_should_skip_stale`
  4 passed; `cargo test -p caco-config --lib` 716 passed; full
  `cargo test -p caco-tui --lib` 2780 passed.

## Diff summary

- Commits: `2135b84b` (8 files changed, 276 insertions).
- Files touched:
  - `crates/caco-config/src/model.rs` — `TtsConfig.max_age_before_skip_secs`
    + serde default (`300`) + overlay propagation.
  - `crates/caco-cli/src/lib.rs` — TTS daemon SSE loop:
    `SpeakableEvent.authored_at`, parse the timestamp in
    `tts_daemon_extract_speakable` preferring the inner
    feed-event payload `ts` over the envelope `ts`, and skip+ack
    stale events in `tts_daemon_loop`. Threshold plumbed from
    config through `dispatch_tts_daemon`.
  - `crates/caco-tui/src/speech.rs` — `SpeechState`
    `tts_max_age_before_skip_secs` field, default 300, seeded by
    `from_speech_config`.
  - `crates/caco-tui/src/state/mod.rs` — `tts_should_skip_stale`
    helper; gates both MessageSpeak / SpeechRequested and
    feed-event `message_sent` / `message_broadcast` /
    `agent_message` enqueue paths.
  - `crates/caco-tui/src/state/tests.rs` — 4 new helper tests.
  - `crates/caco-config/src/validate.rs`,
    `crates/caco-config/tests/config.rs`,
    `crates/caco-daemon/src/audio.rs` — fan-out to add the new
    field on every existing `TtsConfig` literal.
- Tests: +8 (4 caco-cli, 4 caco-tui), -0 / flipped 0.
- Behavioural delta: silently suppresses audible TTS for any
  speak event older than `tts.max_age_before_skip_secs` seconds.
  Acks still flow. Non-audio surfaces (chat, audio-cache replay,
  audit feed) are unchanged.

## Operator-takeaway

If you ever need to disable the new stale-message guard (for
example to deliberately replay history out loud during a demo),
set `speech.tts.max_age_before_skip_secs: 0` in the relevant
config. The TTS daemon and the TUI both honour the same key, and
both still emit a `INFO TTS skipped stale speak message …
age=…s threshold=…s` log line so a missing message is easy to
diagnose.
