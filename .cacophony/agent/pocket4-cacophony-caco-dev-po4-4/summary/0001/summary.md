# Session summary — Audio settings schema gate

## Goal

Land the schema layer (`AgentAudioSettings` + `GlobalMute`/`AudioGlobals`) so the audio-control bead cluster (bd-46eb2b mute logic, bd-cf00b9 TUI, bd-677125 web, bd-7860fd Android, bd-42e5a3 daemon integration) becomes implementable. Without this gate, those beads are blocked: they all assumed schema that didn't exist in caco-config.

## Bead(s)

- `bd-b2b40b` — [stt-ux/audio] Schema gate: AgentAudioSettings + GlobalMute
- (incorporates `bd-5ac69b` — per-agent audio overrides; co-author po4-3 handed me the diff via scratch note `bd-5ac69b-po4-4-handoff` after their unclaim)
- (downstream unblocked: bd-46eb2b, bd-cf00b9, bd-677125, bd-7860fd, bd-42e5a3)

## Before state

- caco-config: no per-agent audio fields, no global-mute concept; only `SpeechConfig::mute: MutePolicy` (TUI startup mute) + `tts_daemon_live_status.muted` (daemon-side runtime mute).
- 5 P1+ audio-control beads all blocked on missing schema.
- po4-3 had a working bd-5ac69b diff (per-agent piece) sitting on a scratch note; they unclaimed and handed it over.
- 760 caco-config lib tests passing.

## After state

- `PerAgentAudioConfig { agents: HashMap<String, AgentAudioOverride> }` on `SpeechConfig.agent_audio`, with `is_muted` / `voice_for` / `voice_filter_for` helpers and overlay-by-id merge.
- `AudioGlobals { global_mute, default_agent_mute }` on `SpeechConfig.audio`, with field-level overlay.
- `SpeechConfig::resolved_agent_audio(agent_id) -> ResolvedAgentAudio` resolves the AC#3 precedence; `is_silenced()` applies `global_mute` always-wins.
- `validate_audio_globals` rejects empty agent_id keys and flags dead-config (global_mute=true coexisting with per-agent unmute).
- 768 caco-config lib tests passing (+8 new).
- Full workspace builds; `cargo test-small` clean.

## Diff summary

- Commit: `920fcaf67`
- Files touched: `crates/caco-config/src/model.rs`, `crates/caco-config/src/validate.rs`, `crates/caco-config/tests/config.rs`, `crates/caco-daemon/src/audio.rs`, `crates/caco-tui/src/speech.rs`, `crates/caco-tui/src/state/tests.rs`
- Tests: +8 (caco-config lib)
- Behavioural delta: zero — schema-only. All new fields are `Option<>` so existing configs deserialise unchanged. Acts as a compile-time gate the downstream wiring beads can now hang behaviour off.

## Operator-takeaway

The bd-b2b40b/bd-5ac69b coordination is the cleanest example I've seen of the new close-discipline rule paying off in practice: po4-3 had overlapping in-flight work, hit the cluster, paused before commit, handed me a diff via scratch note, and unclaimed cleanly. We co-authored a single landed commit (mine) carrying both bead-ids — both beads can be closed against the same origin/main trace, no double-implementation, no lost work. Without the rule, that 20-minute overlap would've shipped two parallel `PerAgentAudioConfig` definitions and someone would've had to revert one. Recommend writing this up as a worked example in the dev profile when the patches review-land.
