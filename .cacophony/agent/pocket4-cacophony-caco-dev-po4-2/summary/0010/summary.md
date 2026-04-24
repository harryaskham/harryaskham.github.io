# Session summary — first-cut per-speak TTS trace

## Goal

Give operators a concrete answer to “why didn’t I hear that?” without
source-diving. The existing TTS surfaces only exposed aggregate counters
(`total_speaks`, `total_failures`, `queue_depth`) and opaque log lines.
This patch adds a bounded per-speak lifecycle trace keyed by `speak_id`
and a `caco tts trace` reader over it.

## Bead(s)

- `bd-98b8d8` — `[tts/observability] Per-speak lifecycle log trail: generated / enqueued / mute-skipped / delivered / played / failed`

## Before state

- Failing tests: none related.
- `caco tts status` showed only aggregates.
- No stable per-speak ID existed for SSE narration events.
- No read-only CLI surface existed for per-speak stage inspection.
- During the TTS debugging swarm, multiple agents had to infer drop
  points indirectly because there was no lifecycle trail.

## After state

- Each `SpeakableEvent` now gets a `speak_id`:
  - upstream `message_id` when present
  - generated `speak-<hex>` otherwise
- The local `caco tts daemon` now keeps a bounded in-memory trace ring:
  - retention: 1 hour
  - hard cap: 5000 events
- New trace stages recorded in the existing runtime loop:
  - `generated`
  - `terminal outcome=skipped rule=stale_age_filter`
  - `mute-check outcome=skipped rule=runtime_mute`
  - `mute-check outcome=passed rule=none`
  - `enqueued`
  - `synth-request`
  - `synth-response outcome=ok`
  - `sink-dispatch`
  - `playback-start`
  - `terminal outcome=played|failed`
- New control endpoint:
  - `GET /api/v1/tts/trace`
- New CLI surface:
  - `caco tts trace`
  - filters: `--speak-id`, `--last`, `--grep`, `--sender`, `--outcome`
  - `--json` supported via the standard CLI path
- Smoke tests remain green: 190 / 190 `cargo test-small`.

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs`
- Tests run:
  - `cargo build -p caco-cli`
  - `cargo test-small`
- Behavioural delta:
  - operators can inspect local TTS daemon lifecycle state per utterance
    instead of only looking at aggregate counters

## Operator-takeaway

This is the “narrowest useful” observability cut: it does not yet add a
persistent DB or every possible mute rule across every upstream layer,
but it gives a concrete local TTS daemon trace with stable IDs and stage
transitions. In practice that means the next TTS debugging session can
answer three important questions quickly:
1. Did the event reach the TTS daemon at all?
2. If yes, was it muted, synthesized, or played?
3. If it failed, at which stage?

That is already a major improvement over today’s counter-only model and
creates a clear scaffold for follow-up trace enrichment if the operator
still wants deeper rule-by-rule logging.
