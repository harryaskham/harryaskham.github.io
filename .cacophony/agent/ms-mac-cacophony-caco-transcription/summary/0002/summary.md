# Session summary — Voice-call routing and live STT capture

## Goal

Implement the first routing slice for Cacophony voice calls across TUI, Web, Android, and future CLI/MCP surfaces, while fixing the live transcription capture regression Harry surfaced where `caco audio transcribe --live` started and then stopped immediately.

## Bead(s)

- `bd-19c6bb` — Implement message routing for voice calls

## Before state

- Failing tests: none known for the touched code before this slice.
- Relevant metrics: `caco audio transcribe --live --input sgu24 --device source.default` printed “Live transcription started” and then stopped immediately in Harry's smoke test.
- Context: the protocol crate already had pure `CallSession` and orchestration helpers for direct DM envelopes, reply playback requests, mode hints, and transcript rendering, but it did not have a single cross-surface router enforcing session/surface scoping.

## After state

- Failing tests: none observed in validation.
- Relevant metrics: a four-second fixed-binary live smoke stayed running until killed by timeout instead of exiting immediately; targeted routing tests and `cargo test-small` passed.
- Context: voice-call routing now has pure Rust session/surface primitives that allow concurrent calls on different surfaces, reject duplicate calls on the same surface, scope STT finals and replies to the right session/agent, and finalize transcripts on call end.

## Diff summary

- Commits: the `bd-19c6bb` implementation commit plus the recorded-summary commits above `origin/main` for this branch.
- Files touched: `crates/caco-stt-protocol/src/voice_call_orchestration.rs`, `crates/caco-cli/src/audio_cmd.rs`, `SPEC.md`
- Tests: added focused unit coverage for cross-surface voice-call routing and live capture command argument shape.
- Validation: `cargo fmt --all -- --check`; `cargo test -p caco-stt-protocol voice_call_orchestration --lib`; `cargo test -p caco-cli live_transcribe_routing_tests --lib`; `CARGO_BUILD_JOBS=2 cargo test-small`.
- Behavioural delta: live transcription no longer passes a literal `-` file argument to `parec`, and unexpected capture stdout closure is reported as a capture error rather than a clean stop.

## Operator-takeaway

The voice-call work now has a shared routing contract that prevents one surface or agent reply from leaking into another live call, and the immediate live-STT stop Harry hit has a tested fix. This is a foundation slice; the remaining UI/daemon surfaces still need to wire these pure routing actions into their concrete call lifecycle paths.
