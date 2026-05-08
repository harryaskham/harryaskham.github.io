# Session summary — Ambient STT transcription timeout budget

## Goal

Continue the STT quality loop after rollout verification remained blocked, capture the newly observed ambient transcription timeout failure as bead work, and reduce dropped ambient transcript evidence caused by the generic daemon request timeout cancelling provider-backed transcription calls.

## Bead(s)

- `bd-722503` — Reduce ambient STT 30s transcription timeouts
- Related waiting bead: `bd-46d5a0` — Deploy ambient STT aggregation to live daemon

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: live ms-mac and sgu24 STT logs showed `request timed out after 30s` while flushing bounded live audio segments; `/api/v1/audio/transcription` still used the generic 30s request middleware budget.
- Context: The earlier 413 payload-size fix allowed bounded live segments to reach the transcription endpoint, but slow provider responses could still be cut off by the daemon before returning useful transcript text.

## After state

- Failing tests: none in focused validation.
- Relevant metrics: `/api/v1/audio/transcription` now resolves through a dedicated `CACO_AUDIO_TRANSCRIPTION_REQUEST_TIMEOUT_SECS` budget, defaulting to 90 seconds and clamped to at least 5 seconds; generic control-plane paths remain at the 30 second default.
- Context: This aligns provider-backed STT with the existing provider-backed TTS timeout pattern without making all daemon requests slower to fail.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-daemon/src/lib.rs`, `SPEC.md`, `README.md`, `AGENTS.md`, `docs/transcription.md`, `docs/transcription.html`.
- Tests: added `audio_transcription_path_gets_extended_request_timeout_bd_722503`.
- Behavioural delta: slow ambient/live STT transcription provider calls should no longer surface as generic `request timed out after 30s` middleware failures before the provider has a chance to respond.

## Operator-takeaway

The STT quality loop found that the post-413 pipeline still had a separate 30-second server timeout bottleneck; transcription now has its own provider-appropriate timeout budget, while `bd-46d5a0` remains waiting for normal daemon rollout and fresh ambient transcript evidence.
