# Session summary — durable local TTS runtime mute

## Goal

Implement `bd-e0bcb2`: local/operator TTS mute intent should survive caco-daemon / caco-tts-daemon restarts instead of unexpectedly returning ms-mac to audible playback.

## Bead(s)

- `bd-e0bcb2` — Persist local TTS mute state across daemon restarts

## Changes

- Updated `crates/caco-cli/src/lib.rs`:
  - `tts_initial_runtime_muted_from_persisted(...)` now restores the persisted daemon runtime mute bit instead of forcing unmuted on startup.
  - `persist_tts_daemon_state(...)` now writes the current runtime mute state (`rt.muted`) instead of always serializing `muted: false`.
  - Replaced the old reset-on-restart regression test with `tts_initial_runtime_mute_restores_persisted_muted_bd_e0bcb2`, covering both persisted muted and persisted unmuted state.
- Updated `crates/caco-tui/src/speech.rs` docs for `TtsDaemonPersistedState::muted` to describe durable operator runtime mute semantics.
- Updated `SPEC.md`, `README.md`, and `AGENTS.md` to state that `caco tts mute` with no scope flags remains the local daemon runtime mute path but persists across daemon/TTS restarts until explicitly unmuted, while config-backed scoped rules remain separate.

## Validation

- `rustfmt --edition 2021 --check --config skip_children=true crates/caco-cli/src/lib.rs crates/caco-tui/src/speech.rs` — passed after formatting.
- `git diff --check` — passed.
- `cargo test -p caco-cli tts_initial_runtime_mute_restores_persisted_muted_bd_e0bcb2 -- --test-threads=1` — passed.
- `cargo clippy -p caco-cli --lib --no-deps -- -D warnings` — passed.

## Notes

- This slice changes local runtime mute durability. Config-backed scoped mute rule writing/inspection remains on the existing `caco tts mute --node/--project/...`, `caco tts list-mutes`, and `caco tts status --explain` path.
