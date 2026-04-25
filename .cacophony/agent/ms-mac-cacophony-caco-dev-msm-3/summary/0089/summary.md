# Session summary — TTS speech endpoint timeout budget

## Goal

Address `bd-943501`, where ms-mac TTS was otherwise healthy but one speak failed with the daemon's generic `request timed out after 30s` envelope while the headless TTS daemon expected a longer synthesis budget for Azure speech.

## Bead(s)

- `bd-943501` — [TTS] Azure speech API intermittent 500 timeouts on ms-mac

## Before state

- Failing tests: none for this scope; unrelated broken-on-main clippy/docs issues were already owned by other agents.
- Relevant metrics: live ms-mac TTS status was unmuted, `output_routing=local-device`, queue-draining, and recent traces showed `sink=local-device detail=device=MacBook Pro Speakers` with RMS/peak evidence, but `last_failure_summary` showed `speech API error: 500 Internal Server Error ... request timed out after 30s`.
- Context: `caco-cli` already used a 60s headless TTS audio request timeout, but `caco-daemon` applied the generic 30s request-timeout middleware to `POST /api/v1/audio/speech`, so provider-backed synthesis could be cancelled before the TTS daemon's own bounded timeout completed.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: `cargo test -p caco-daemon audio_speech_path_gets_extended_request_timeout`, `cargo test -p caco-daemon audio_speech_timeout_has_dedicated_env_override`, and `cargo check -p caco-daemon --tests` passed.
- Context: `POST /api/v1/audio/speech` now has a dedicated bounded middleware timeout: default 90s, overridable with `CACO_AUDIO_SPEECH_REQUEST_TIMEOUT_SECS`. Ordinary control-plane endpoints still use the existing 30s default.

## Diff summary

- Commits: `4cb252f36`
- Files touched: `crates/caco-daemon/src/lib.rs`, `SPEC.md`, `README.md`
- Tests: +2 daemon timeout helper tests.
- Behavioural delta: transient Azure/OpenAI speech latency should no longer surface as the generic 30s middleware timeout before the TTS daemon's 60s audio request budget can resolve.

## Operator-takeaway

ms-mac audio routing is still green; this fixes a separate reliability edge where slow provider synthesis could make a spoken message fail even though the local speaker path was healthy.
