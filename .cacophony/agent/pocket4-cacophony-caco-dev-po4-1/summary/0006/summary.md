# Session summary — per-agent audio controls in TUI

## Goal

Add per-agent audio controls to the TUI Audio view so the operator
can see which agents are muted, their voice assignments, and
silenced state at a glance — matching the web panel shipped in
bd-677125.

## Bead(s)

- `bd-cf00b9` — Implement per-agent audio controls in TUI (P1 feature)

## Before state

- TUI Audio view had TTS, STT, Output/Input Routing, and TTS Daemon
  sections but no per-agent breakdown.
- SpeechState did not carry `agent_audio` or `audio_globals` from the
  config — callers had to pass through `SpeechConfig` directly.

## After state

- SpeechState has `agent_audio: Option<PerAgentAudioConfig>` and
  `audio_globals: Option<AudioGlobals>`, populated from config on
  startup and live reload.
- Audio view renders a "Per-Agent Audio" section with:
  - Global Mute + Default Mute status line
  - Per running agent: 🔇/🔊 icon, short agent id, mute state, voice
- All 2915 caco-tui lib tests + 182 test-small pass.

## Diff summary

- `crates/caco-tui/src/speech.rs` (+12 lines: fields, defaults, population)
- `crates/caco-tui/src/views/audio.rs` (+92 lines: Per-Agent Audio section)

## Operator-takeaway

Read-only, same as the web counterpart. Shows resolved audio state
from config. Mutation (toggle per-agent mute from TUI keybinds) is
a follow-up requiring runtime state persistence.
