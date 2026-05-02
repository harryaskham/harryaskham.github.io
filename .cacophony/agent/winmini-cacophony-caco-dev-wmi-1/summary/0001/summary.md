# Session summary — bd-3dacb5 sender metadata for TTS effects

## Goal

Fix the daemon-side TTS effect environment so command-template wrappers can see the authenticated speaking sender and announce a meaningful agent/project name instead of falling back to the useless "alpha golf echo" placeholder path.

## Bead(s)

- `bd-3dacb5` — Pass sender metadata to TTS effects so agent-name prefixes are meaningful

## Before state

- Failing tests: none in the targeted path, but the runtime behavior was wrong.
- Relevant metrics: TTS effect wrappers under `.cacophony/tts/effects/*.yaml` already tried to derive a spoken prefix from `CACO_AGENT_ID`, `CACO_AGENT_KIND`, `CACO_PROJECT`, and `CACO_NODE`, but daemon-side command-template execution never injected the speaking sender metadata. In that path the wrappers fell back to literal `agent` and NATO-spelled the first three characters into `alpha golf echo`.
- Context: the smallest honest fix was to keep the effect wrappers and command-template mechanism intact, then make the daemon populate the sender metadata they were missing.

## After state

- Failing tests: none in the focused caco-daemon lane.
- Relevant metrics: `crates/caco-daemon/src/audio.rs` now resolves speaking-sender metadata once per TTS request and injects both explicit `CACO_SPEAK_*` env vars and compatibility aliases (`CACO_AGENT_ID`, `CACO_AGENT_KIND`, `CACO_PROJECT`, `CACO_NODE`) into command-template execution. `crates/caco-config/src/model.rs` now documents those env vars in the TTS command-template contract.
- Context: existing first-party effect wrappers can now prepend a meaningful sender identity from authenticated daemon context instead of dropping to the misleading generic fallback.

## Diff summary

- Commits: `d76876c07`
- Files touched: `crates/caco-daemon/src/audio.rs`, `crates/caco-config/src/model.rs`
- Tests: `cargo test -p caco-daemon run_tts_command_template_reads_output_file -- --nocapture`; `cargo test -p caco-daemon run_tts_command_template_injects_sender_metadata_bd_3dacb5 -- --nocapture`; `cargo build -p caco-daemon`
- Behavioural delta: daemon-owned TTS command-template effects now receive the actual speaking sender’s agent id, project, node, kind, and display-name metadata, so effect-generated prefixes can identify the real speaker instead of announcing `agent` / `alpha golf echo`.

## Operator-takeaway

The core bug was not in the effect wrappers themselves but in the daemon environment they run under: once the authenticated sender metadata is injected into command-template execution, the existing effect logic has enough context to produce useful spoken prefixes again.
