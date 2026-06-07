# Session summary — Agent-DM TTS readout

## Goal

Implement bd-601a19 by adding a default-disabled, configurable TTS readout path for agent-to-agent direct messages, with the Cacophony deployment opting in at a faster speed while preserving existing TTS mute, solo/focus, voice, filter, and daemon/TUI duplicate-suppression behaviour.

## Bead(s)

- `bd-601a19` — Add configurable TTS readout for agent-to-agent DMs

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: direct `message_sent` feed events were not speakable by the headless TTS daemon unless they were explicit `message_speak` events; TUI broad `read_messages_aloud` was the only frontend readout path.
- Context: operator requested an opt-in `speech.tts.agent_dms` overlay that inherits ordinary TTS defaults but can override event-class settings such as speed.

## After state

- Failing tests: none in the focused validation listed below.
- Relevant metrics: `speech.tts.agent_dms.read_aloud` defaults to false; when enabled, TUI and `caco-tts-daemon` both extract/enqueue direct `message_sent` events and apply an optional `agent_dms.speed` override.
- Context: `.cacophony/config.yaml` enables agent-DM readout for the Cacophony operator environment with `speed: 1.5`, while generic defaults remain disabled.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `.cacophony/config.yaml`, `SPEC.md`, `README.md`, `AGENTS.md`, `crates/caco-config/src/model.rs`, `crates/caco-config/src/validate.rs`, `crates/caco-cli/src/lib.rs`, `crates/caco-cli/src/tts_daemon.rs`, `crates/caco-daemon/src/audio.rs`, `crates/caco-tui/src/app.rs`, `crates/caco-tui/src/speech.rs`, `crates/caco-tui/src/state/mod.rs`, `crates/caco-tui/src/state/tests.rs`.
- Tests: added focused config, TTS daemon extraction, and TUI state readout tests; no tests removed.
- Behavioural delta: direct agent DMs remain silent by default, but configured nodes can read them aloud with inherited TTS settings and a speed override. The TUI uses per-item speed overrides and still defers feed TTS when the daemon owns playback to avoid duplicates.
- Validation run: `cargo test -p caco-cli tts_daemon_agent_dm --lib`; `cargo test -p caco-tui agent_dm --lib`; `cargo test -p caco-config tts_agent_dms --lib`; `cargo check -p caco-config --lib`; `cargo check -p caco-cli --lib`; `cargo check -p caco-tui --lib`; `cargo check -p caco-daemon --lib`; `cargo clippy -p caco-config --lib -- -D warnings`; `cargo clippy -p caco-cli --lib -- -D warnings`; `cargo clippy -p caco-tui --lib -- -D warnings`; `cargo clippy -p caco-daemon --lib -- -D warnings`; `caco config validate --project-config-dir .cacophony`; `caco --config .cacophony/config.yaml config validate --show-materialized`; `git diff --check`. `scripts/rustfmt-changed.sh` verified already-clean touched TUI files and intentionally skipped pre-existing rustfmt drift in large crate-root files to avoid unrelated formatting churn.

## Operator-takeaway

Agent-to-agent DMs can now be made audible without turning on every incoming-message readout: use `speech.tts.agent_dms.read_aloud` plus optional `speed`, and normal TTS policy/diagnostics still explain why a message did or did not play.
