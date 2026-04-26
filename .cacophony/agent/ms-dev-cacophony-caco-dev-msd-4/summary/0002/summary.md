# Session summary — caco-audio live transcription MCP controls

## Goal

Expose first-party, non-blocking live transcription controls through the `caco audio` command tree and a scoped `caco-audio` plugin so agents can start, stop, and inspect live STT capture through generated MCP tools instead of monopolizing a blocking CLI process.

## Bead(s)

- `bd-929911` — Expose first-party live transcription session control through caco-audio MCP/plugin

## Before state

- Failing tests: none for this bead.
- Relevant metrics: `caco audio transcribe --live` was a blocking foreground capture loop; no scoped `plugins/caco-audio` package existed; generated audio MCP metadata exposed one-shot audio commands but not `live start/stop/status` controls.
- Context: live transcription already used the daemon transcription endpoint, but agent-facing partial/final transcript markers and non-blocking lifecycle state were missing.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: `caco audio live start|stop|status` now exist in CLI/help and generated MCP metadata; `live start` spawns `audio transcribe --live --agent-buffer` in the background, persists PID/log/scratch metadata, connects the default scratch note to the current agent when possible, and `stop` sends SIGINT for graceful finalization.
- Context: live sessions append `[partial transcription] ...` for segments and `[user transcription] ...` at final stop, while preserving the existing daemon `/api/v1/audio/transcription` path and honoring configured live input routing, including named PulseAudio sources such as `source.default`, plus `--input` / `--device` overrides.

## Diff summary

- Commits: `0c3e6922b`
- Files touched: `crates/caco-cli/src/audio_cmd.rs`, `crates/caco-cli/src/lib.rs`, `plugins/caco-audio/.claude-plugin/plugin.json`, `.claude-plugin/marketplace.json`, `crates/caco-profile/src/bridge.rs`, `crates/caco-profile/src/canonical.rs`, `crates/caco-profile/tests/profile.rs`, `crates/caco-daemon/build.rs`, `crates/caco-daemon/src/agent/tests.rs`, `SPEC.md`, `README.md`, `AGENTS.md`
- Tests: `cargo fmt --all -- --check`; `cargo test -p caco-cli audio_live --lib`; `cargo test -p caco-cli marketplace_json_is_valid --lib`; `cargo test -p caco-cli per_plugin_json_manifests_exist --lib`; `cargo test -p caco-profile canonical_mcp_servers_match_bridge_arms --lib`; `cargo test -p caco-daemon known_mcp_servers_are_in_sync_with_build_rs --lib`; `cargo check -p caco-cli`; `git diff --check`
- Behavioural delta: agents can now call `caco_audio_live_start`, `caco_audio_live_status`, and `caco_audio_live_stop` via the caco-audio MCP family and receive agent-turn transcript markers through connected scratch context.

## Operator-takeaway

The STT live-capture path is now controllable as first-party MCP lifecycle rather than an interactive terminal-only loop, which is the foundation for `/stt`-style voice input that agents can start and stop autonomously without losing transcript context.
