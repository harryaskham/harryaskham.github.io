# Session summary — bd-53a0f3 STT live-transcription visual indicator state machine

## Goal

`caco audio transcribe --live` (and TUI/web/macOS) needs to surface
5 visual states to operators within 100ms of model state changes:
Idle / Speech / Transcribing / Partial / Final / Error. This bead's
state machine is the shared contract every renderer reads.

## Bead(s)

- `bd-53a0f3` — Visual indicators for live transcription state +
  voice-call e2e smoke (P1, operator-asked)
- (parent epic `bd-9496d1` STT hardening)
- (depends on bd-71ce98 engine MVP, bd-a55d88 voice-call session,
  bd-68b76d corpus harness — bd-a55d88 done; e2e smoke deferred)

## Before state

- No shared indicator state machine. Every UI surface (TUI, web,
  macOS) would have invented its own.

## After state

- New `crates/caco-stt-protocol/src/indicator.rs` (~485 lines)
- `IndicatorState { Idle, Speech, Transcribing, Partial, Final, Error }`
  with `is_healthy()` + `css_class()` (returns "stt-{state}" stable
  per state — DOM/TUI/SwiftUI all key off the same string)
- `Indicator` state machine — pure data, no I/O:
  - `on_vad_speech(level, now_ms)` — Idle→Speech; clamps level to
    [0,1]; bumps seq each frame so the level meter animates
  - `on_vad_silence(now_ms)` — Speech→Transcribing or Final→Idle
  - `on_event(stream_event, now_ms)` — Partial/Final/Error from engine
  - `on_classified_error(error, now_ms)` — for already-classified
    errors that didn't go through `StreamEvent::Error`
  - `clear_error(now_ms)` — Error→Idle (operator dismissed or
    engine recovered)
- `IndicatorSnapshot { state, level, partial_text, last_final,
  error, seq, last_change_ms }` — denormalised for renderers
- `within_latency_budget(now, last_change, budget)` helper for
  tests + UIs to assert criterion 1's 100ms invariant
- `DEFAULT_LATENCY_BUDGET_MS = 100`

## Diff summary

- Files: 2 modified — `src/lib.rs` (+1 module decl) — and 1
  created — `src/indicator.rs`
- Tests: +22 / -0 (caco-stt-protocol total: 108 in 0.02s)
- Behavioural delta: zero — pure addition

## Acceptance status

- [x] Criterion 1: 5 visual states + state-pill model with
  100ms latency invariant (DEFAULT_LATENCY_BUDGET_MS) and
  `within_latency_budget` helper renderers can assert against
- [x] Criterion 2: state-transition tests on synthesized inputs
  (`full_utterance_lifecycle` covers silence → speech →
  transcribing → partial → final → idle)
- [ ] Criterion 3: e2e voice-call smoke in CI — DEFERRED, blocked
  on bd-71ce98 (engine MVP) + bd-68b76d (corpus harness). State
  machine is ready to consume real engine events.
- [x] Criterion 4: every error state has a one-line fix (via
  `SttError.fix_hint` from bd-4552dc doctor module — wired in
  `on_event`'s `StreamEvent::Error` handler)

## Operator-takeaway

The state machine ships now; the e2e CI smoke waits for the
engine. Renderers (TUI/web/macOS) can already wire against the
indicator and ship their visual layer:

```rust
let mut indicator = Indicator::new();
// ...wire engine events + VAD frames to the methods...
let snap = indicator.snapshot();
// renderer reads snap.state, snap.level, snap.partial_text, etc.
```

The CSS-class function gives every renderer the same stable
string-keying so the "traffic-light" colour is uniform across
surfaces. UIs that want to assert criterion 1's 100ms freshness
call `within_latency_budget(now, snap.last_change_ms, 100)`.

## Why I'm unclaiming, not closing

Acceptance criterion 3 (e2e smoke in CI) is genuinely blocked on
bd-71ce98 (engine MVP) which doesn't exist yet. Per close-discipline,
I'm landing the state-machine layer, appending this progress note
to the bead, and unclaiming so the agent that lands bd-71ce98 can
wire the e2e smoke without conflict.
