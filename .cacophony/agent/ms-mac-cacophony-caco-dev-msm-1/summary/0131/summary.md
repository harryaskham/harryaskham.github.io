# Session summary — scoped TTS mute runtime policy

## Goal

Move scoped `caco tts mute/unmute --node/--project/...` controls out of mutable config writes and into an explicit runtime policy layer so routine operator mute controls do not corrupt import-wrapper installs or require declarative config churn.

## Bead(s)

- `bd-6a1e1d` — Move scoped TTS mute rules out of live config into runtime state

## Before state

- Failing tests: release/controller evidence showed scoped `caco tts mute --node ms-mac` could append `speech.audio.by_node` into the selected live config wrapper and silence most agents.
- Relevant metrics: no runtime scoped audio policy file existed; `caco tts list-mutes` only reported config-backed policy; daemon agent-audio resolution only used `SpeechConfig` from config.
- Context: unscoped TTS daemon mute already persisted in `$CACOPHONY_DIR/tts-daemon/state.json`, but scoped mutes used `write_validated_config_yaml`, which is unsafe for repo-import-wrapper configs.

## After state

- Failing tests: none known for this slice.
- Relevant metrics: final-tree `tj-025acd64` passed `cargo test -p caco-config runtime_audio_policy -- --test-threads=2`; `tj-5a8d4e06` passed the caco-cli import-wrapper regression test; `tj-dd4698bd` passed `cargo check -p caco-daemon -p caco-cli`; `docs/validate-pages.sh` passed after updating the macOS docs sibling marker.
- Context: scoped TTS mute/unmute now writes `$CACOPHONY_DIR/tts-daemon/audio-policy.json` by default; daemon resolution reads that runtime file on each relevant request and overlays it after declarative config; `--persist-config` remains the explicit opt-in for declarative policy changes.

## Diff summary

- Commits: `1a38d3b5d`.
- Files touched: `crates/caco-cli/src/lib.rs`, `crates/caco-config/src/model.rs`, `crates/caco-config/src/paths.rs`, `crates/caco-daemon/src/lib.rs`, `crates/caco-daemon/src/audio.rs`, `README.md`, `SPEC.md`, `AGENTS.md`, `docs/cli.html`, `docs/macos-development.md`, `docs/macos-development.html`.
- Tests: +2 focused regressions; changed no existing test expectations beyond formatting.
- Behavioural delta: scoped mute commands no longer edit config by default, list/status surfaces separate runtime policy from declarative policy, and daemon audio gates honor runtime policy without restart.

## Operator-takeaway

Scoped TTS mutes are now runtime controls, not config edits. Operators can temporarily mute by node/project/agent without damaging import-wrapper config, while declarative policy still exists behind an explicit `--persist-config` path.
