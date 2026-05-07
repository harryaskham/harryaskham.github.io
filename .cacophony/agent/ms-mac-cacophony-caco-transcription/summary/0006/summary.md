# Session summary — bound live STT segment payloads

## Goal

Continue bd-930017 after the daemon lifecycle fixes landed by addressing the remaining transcript-emission blocker: live capture could stay in a single oversized VAD segment and hit the daemon transcription endpoint payload limit instead of producing bounded transcript entries.

## Bead(s)

- `bd-930017` — Make configured ms-mac and sgu24 STT daemons produce transcripts

## Before state

- Failing tests: none known before this slice; runtime verification showed failures instead.
- Relevant metrics: ms-mac and sgu24 STT daemons were alive with `capture_running=true`, but both `caco stt diff` buffers were empty.
- Context: ms-mac STT logs showed repeated old `parec` errors followed by `HTTP 413 Payload Too Large` from live transcription, meaning long continuous capture could be dropped instead of being emitted as transcript entries.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: live STT now caps each VAD segment at 8 seconds or 240,000 raw PCM bytes, whichever is smaller, before sending it to the transcription endpoint.
- Context: the daemons still require audible/non-silent input for actual transcript entries, but the oversized-segment path is bounded so ambient capture should no longer wedge on a giant HTTP 413 segment.

## Diff summary

- Commits: `e6a1a236b`.
- Files touched: `crates/caco-cli/src/audio_cmd.rs`, this summary.
- Tests: +1 focused unit test; validation passed `tj-ddf6a4bf` and `tj-552a56c0`; `rustfmt --edition 2021 --check crates/caco-cli/src/audio_cmd.rs` and `git diff --check` passed locally.
- Behavioural delta: `caco audio transcribe --live` now flushes live speech segments once they reach a safe bounded size and reuses one helper for normal, bounded, and final flush emission.

## Operator-takeaway

The STT daemon lifecycle is now healthy and the remaining live-transcription payload issue is bounded in code; if transcript buffers remain empty after this lands, the next evidence points at actual input/silence/permission state rather than daemon process or oversized-segment plumbing.
