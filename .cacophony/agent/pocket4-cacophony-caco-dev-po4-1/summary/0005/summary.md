# Session summary — per-agent audio controls in web app

## Goal

Ship a per-agent audio controls panel in the web dashboard so
operators can see at a glance which agents are muted, which voice
they're using, and whether the global mute kill-switch is active —
leveraging the bd-b2b40b audio schema that just landed.

## Bead(s)

- `bd-677125` — Implement per-agent audio controls in web app (P1 feature)

## Before state

- TTS panel had only "Browser" and "Daemon" sections.
- No way to see per-agent mute/voice state from the web UI.
- bd-b2b40b audio schema (AgentAudioSettings, GlobalMute, per-agent
  overrides) existed in config but had no web-facing read surface.

## After state

- New daemon endpoint `GET /api/v1/speech/agent-audio` returns
  global_mute, default_agent_mute, and a per-agent map with mute,
  global_mute, silenced, voice, and project fields. Uses
  `SpeechConfig::resolved_agent_audio()` for the bd-b2b40b
  precedence chain.
- TTS panel now has a third section "Per-Agent Audio" with a table
  showing each running agent's resolved state. Includes badge
  styling (muted/on) and emoji indicators (🔇/🔊).
- `cargo check -p caco-daemon -p caco-web` clean.
- `cargo test -p caco-web --lib` 182/182 pass.

## Diff summary

- Commits: `5ba3fc794`
- Files touched:
  - `crates/caco-daemon/src/lib.rs` (+58 lines: endpoint + route)
  - `crates/caco-web/static/app.js` (+55 lines: loadPerAgentAudio + UI)
  - `crates/caco-web/static/style.css` (+30 lines: table styling)
- Tests: no new tests (read-only endpoint, table rendering).

## Operator-takeaway

This is the read-only half. The web UI now shows resolved per-agent
audio state but cannot mutate it. The mutation side (toggle mute per
agent from the web) requires either a config-file-write path or
daemon-side runtime state with SQLite persistence — both are more
complex than a single bead. Filed as follow-up scope. The existing
`caco tts mute` / config YAML is still the mutation surface until
then.
