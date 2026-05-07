# Session summary — STT-arrival transcript narrator wake

## Goal

Replace the transcript-narrator's noisy fixed polling workaround with a first-party event-triggered wake path from `caco-stt-daemon` transcript arrival, while preserving the narrator's read-only safety boundaries and multi-source STT behavior.

## Bead(s)

- `bd-b1ddea` — Trigger transcript narrator from STT arrival instead of fixed polling

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: `transcript-narrator` profile documented a 10s Pi `/loop` polling cadence; operator reported 10s was too noisy and 60s was still a workaround.
- Context: STT daemons already expose cursor-based diffs and the narrator stores per-source cursors, but nothing woke the narrator only when source cursors advanced.

## After state

- Failing tests: none in focused validation.
- Relevant metrics: STT daemon supports configured `transcript_wake_project` and `transcript_wake_targets`; checked-in `ms-mac` and `sgu24` STT daemon configs target `helsinki-cacophony-transcript-narrator` in project `cacophony`.
- Context: Each recorded utterance can now send a bounded first-party Cacophony wake message, rate-limited per daemon burst, telling the narrator to read configured `caco stt diff` sources from persisted cursors. The wake body explicitly says transcript text is not a command channel.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `.cacophony/config.yaml`, `.cacophony/profiles/transcript-narrator.md`, `SPEC.md`, `README.md`, `AGENTS.md`, `crates/caco-cli/src/audio_cmd.rs`, `crates/caco-cli/src/lib.rs`, `crates/caco-config/src/model.rs`, `crates/caco-config/src/validate.rs`, `crates/caco-sidecar/src/lifecycle.rs`, `docs/transcription.md`, `docs/transcription.html`.
- Tests: added/updated focused coverage for STT wake target parsing/prompt safety, config parsing, CLI help metadata, and lifecycle CLI arg propagation.
- Behavioural delta: transcript consumers no longer need a fixed 10s/60s Pi polling loop for normal narrator operation; a conservative 15m fallback remains documented only when event delivery is unavailable.

## Operator-takeaway

The narrator wake path is now event-driven from STT cursor advancement rather than prompt-loop polling, but it remains a nudge to read diffs — ambient transcript text is still not treated as instructions or permission to mutate Cacophony state.
