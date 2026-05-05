# Session summary — voice transcription delivery fix

## Goal

Fix the operator-reported failure where voice instructions could be transcribed by the foreground CLI path but not reliably delivered into agent panes or scratchpads from the UI/agent-buffer flows.

## Bead(s)

- `bd-026f90` — Fix voice instruction transcription delivery to agents and scratchpads

## Before state

- Failing tests: no scoped failing tests existed for this exact regression.
- Relevant metrics: Harry proved `caco audio transcribe --live --input sgu24 --device source.default` produced live transcript segments, while `transcription:live` scratchpad was absent and the prior log showed `local-device selected for live transcription but no --device or configured input device was provided` for the stale live session.
- Context: TUI scratchpad transcription used a legacy placeholder dictation path rather than the same real record/transcribe route, TUI PulseAudio recording still passed a literal `-` to `parec`, voice-mode async pane delivery could clear the target after injection, and foreground `--agent-buffer <agent-id>` was parsed but not mapped to a target live scratchpad.

## After state

- Failing tests: `cargo fmt --all -- --check` still fails on unrelated pre-existing formatting drift in untouched files; filed `bd-1c21ad` for that broken-on-main validation blocker.
- Relevant metrics: focused queued tests passed for scratchpad delivery, voice target retention, CLI agent-buffer target mapping, and TUI PulseAudio command parity; docs validation passed with 3313 passed, 0 warnings, 0 failed.
- Context: Foreground live transcription can map `--agent-buffer <agent-id>` to `transcription:<agent-id>:live`, TUI PulseAudio capture now matches the working CLI no-output-file path, scratchpad transcription saves immediately, and voice-mode async delivery preserves/re-arms the target.

## Diff summary

- Commits: `5a8c75279`
- Files touched: `crates/caco-cli/src/audio_cmd.rs`, `crates/caco-cli/src/lib.rs`, `crates/caco-tui/src/app.rs`, `crates/caco-tui/src/recording.rs`, `SPEC.md`, `docs/transcription.md`, `docs/transcription.html`
- Tests: added 4 focused regression tests / updated 1 existing PulseAudio command assertion.
- Behavioural delta: Voice instructions now use the proven live transcription semantics for TUI capture and agent-buffer routing instead of dropping into placeholder scratchpad state or clearing async voice targets.

## Operator-takeaway

Harry’s CLI proof was the key: the foreground live PulseAudio path worked, so the fix aligns the UI and agent-buffer paths with that known-good route and records tests for each delivery gap that caused voice instructions to disappear.
