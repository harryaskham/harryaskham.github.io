# Session summary — config-backed TTS mute dimensions and explain surface

## Goal

Make TTS mute configuration operator-comprehensible and configurable along the
actual axes Harry asked for, without breaking the existing emergency
kill-switch semantics. The target was a first useful slice that preserves the
existing local daemon runtime mute, adds config-backed mute dimensions, and
lets an operator answer “why didn’t I hear that?” for a live agent via CLI.

## Bead(s)

- `bd-7f774e` — `[tts/config] Multi-dimensional mute controls: by project, by surface/profile, by agent name, by node — CLI + config surface`

## Before state

- Failing tests: none related, but the full suite remained intentionally out of scope; smoke only.
- `speech.audio` only supported:
  - `global_mute`
  - `default_agent_mute`
- `speech.agent_audio.agents.<agent_id>.mute` existed, but only as a raw config knob.
- `SpeechConfig::resolved_agent_audio(agent_id)` only knew:
  - global kill-switch
  - per-agent-id override
  - default-agent-mute fallback
- `caco tts mute` / `unmute` only controlled the local TTS daemon runtime mute.
- `caco tts status` had no explain mode.
- No first-party `caco tts list-mutes` surface existed.
- Operator clarification for current topology:
  - `helsinki` is the intended unmuted daemon
  - `sgu24` is the Pulse sink the operator listens to
  - other daemons being muted is intentional today

## After state

- Config model now supports config-backed mute dimensions under `speech.audio`:
  - `by_project`
  - `by_node`
  - `by_surface`
  - `by_profile`
  - `by_agent_role`
  - `by_agent_name`
- Each dimension accepts compact boolean rules via `ScopedMuteRule`.
- Resolver now supports live agent context with documented precedence:
  - `speech.audio.global_mute` (hard kill-switch)
  - `speech.agent_audio.agents.<agent_id>.mute`
  - `speech.audio.by_agent_name.<name>`
  - `speech.audio.by_profile.<profile>`
  - `speech.audio.by_agent_role.<role>`
  - `speech.audio.by_project.<project>`
  - `speech.audio.by_node.<node>`
  - `speech.audio.by_surface.<surface>`
  - `speech.audio.default_agent_mute`
  - built-in default `false`
- The resolver now returns the winning matched rule for explain/debug paths.
- The daemon now uses live agent context when resolving mute decisions in:
  - message speak suppression path
  - direct audio / transcription proxy mute gates
  - `/api/v1/speech/agent-audio`
- `/api/v1/speech/agent-audio` now returns:
  - the dimension maps
  - per-live-agent `matched_rule`
  - `node`, `profile`, `agent_name`, and `role`
- CLI additions:
  - `caco tts mute --project|--node|--surface|--profile|--role|--name|--agent ...`
  - `caco tts unmute ...` with the same scoped config-backed targets
  - `caco tts status --explain <agent-id>`
  - `caco tts list-mutes`
- CLI behavior split is explicit:
  - no scope flags → local daemon runtime mute/unmute
  - one scope flag → config-backed mute/unmute rule persisted into config
- Config mutation path validates after write and rolls back on invalid config.
- Docs updated in:
  - `README.md`
  - `AGENTS.md`
  - `SPEC.md`

## Diff summary

- Files touched:
  - `crates/caco-config/src/model.rs`
  - `crates/caco-config/src/validate.rs`
  - `crates/caco-daemon/src/lib.rs`
  - `crates/caco-daemon/src/audio.rs`
  - `crates/caco-cli/src/lib.rs`
  - `README.md`
  - `AGENTS.md`
  - `SPEC.md`
- Validation:
  - `cargo build -p caco-cli`
  - `cargo test-small`
- Tests/coverage added:
  - resolver precedence test: profile over project
  - audio globals overlay test for dimension-map merge behavior
  - validation now rejects empty dimension keys and dead `false` dimension rules under `global_mute: true`
- Behavioural delta:
  - operators can now inspect and persist mute rules along the requested axes without source-diving
  - daemon-side speak suppression can now honor richer context than agent-id alone

## Operator-takeaway

This lands the first practical “explainable mute policy” surface. The important
thing is not just the new config keys — it is that the daemon, CLI, and docs
now agree on one precedence model, and `caco tts status --explain <agent-id>`
can surface the winning rule. That directly addresses the earlier ambiguity
around why audio was (or was not) heard on the intended `helsinki -> sgu24`
listening topology.
