# Session summary — TTS speech timeout budget alignment

## Goal

Fix the passive ms-mac TTS failure mode where local speech playback reported `speech request failed after 60s timeout window` even though the daemon-side speech endpoint was allowed to wait longer for provider synthesis. The goal was to use existing logs and code inspection only, respecting the operator direction not to run new TTS probes.

## Bead(s)

- `bd-3b37f8` — [TTS] Local audio speech endpoint times out on ms-mac

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: passive traces showed multiple ms-mac/local TTS terminal failures after the headless daemon's 60s `/api/v1/audio/speech` client timeout.
- Context: `POST /api/v1/audio/speech` already had a daemon-side 90s synthesis timeout via `CACO_AUDIO_SPEECH_REQUEST_TIMEOUT_SECS`, but the headless TTS daemon still cancelled its client request at a default 60s.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: default headless TTS audio request timeout is now 85s, matching the 90s speech endpoint budget minus a 5s cancellation margin; explicit `CACO_TTS_AUDIO_TIMEOUT_SECS` overrides are preserved.
- Context: the implementation avoids active audio probes and instead removes the timeout-budget mismatch that caused local playback to give up while server-side synthesis could still be legitimately in flight.

## Diff summary

- Commits: source branch commit `303494846` before final summary text normalization; reintegration will squash this into a mainline commit.
- Files touched: `crates/caco-cli/src/lib.rs`, `SPEC.md`, `README.md`, `AGENTS.md`, `docs/api.html`, `docs/daemon.html`, `docs/transcription.md`, `docs/transcription.html`.
- Tests: updated/added 3 focused timeout-resolution unit tests; no tests removed.
- Behavioural delta: `tts_daemon_audio_request_timeout()` now derives from `CACO_AUDIO_SPEECH_REQUEST_TIMEOUT_SECS - 5s` by default and uses 85s when no env overrides are set, instead of falling back to 60s or the generic request timeout path.
- Validation: `cargo fmt --all -- --check`; `cargo test -p caco-cli tts_daemon_audio_request_timeout --lib`; `docs/validate-pages.sh`; `git diff --check`; `cargo check -p caco-cli`.

## Operator-takeaway

The ms-mac TTS endpoint issue was a local timeout-budget mismatch, not evidence that new probe spam was needed: the playback daemon cancelled at 60s while the speech endpoint was intentionally permitted to wait 90s for provider-backed synthesis. The landed change aligns those budgets and documents the contract so future TTS timeout edits keep both sides in step.
