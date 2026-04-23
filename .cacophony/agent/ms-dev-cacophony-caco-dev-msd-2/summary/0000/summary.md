# Session summary — bd-a17114 caco-stt-protocol crate (streaming STT wire contract)

## Goal

Land the **wire contract** for the streaming STT subsystem so the
parallel children of bd-9496d1 (xplat STT epic) can land independently
without coordinating SHAs: the engine MVP (bd-71ce98), the WER bench
(bd-68b76d), the grammar-bias work (bd-c1930c), and `caco voice call`
(bd-a55d88) all consume the same JSONL shape.

## Bead(s)

- `bd-a17114` — Streaming transcript protocol contract (P1)
- (parent epic: `bd-9496d1` xplat STT + voice-call-with-controller)
- (peers consuming this contract: `bd-71ce98` MVP engine,
  `bd-a55d88` voice-call duplex, `bd-68b76d` WER harness,
  `bd-c1930c` grammar bias)

## Before state

- No crate, no types, no tests for the streaming STT wire format
- Existing `crates/caco-daemon/src/scribble_stt.rs` covers a non-
  streaming local-Whisper transcribe call; not a streaming wire
  protocol

## After state

- New `crates/caco-stt-protocol/` workspace member, zero-engine deps
  (serde + serde_json + thiserror only). Library compiles standalone
  in a couple seconds; full workspace builds clean.
- `StreamEvent` enum: `Started { protocol, engine, sample_rate }`,
  `Partial { seq, text, t_start_ms, t_end_ms }`, `Final {…}`,
  `Silence { seq, duration_ms, t_end_ms }`, `Error { code, message }`,
  `Stopped { reason: StopReason }`
- `StreamCommand` enum: `Reset`, `Stop` — written to engine stdin
  one JSONL per line
- `StopReason` enum: `Requested`, `InputClosed`, `Error`
- Constants: `PROTOCOL_VERSION = 1`, `latency::MEDIAN_PARTIAL_MS_TARGET
  = 300`, `latency::P95_PARTIAL_MS_TARGET = 600` (documenting
  bd-a17114 acceptance criterion 4)
- Helpers: `serialize_event`, `serialize_command`, `parse_event`,
  `parse_command`, `parse_event_lenient` (forward-compat: unknown
  `type` yields `Ok(None)`), `event_has_transcript`,
  `transcript_payload`, `is_terminal`
- 19 unit tests covering per-variant round-trip, lenient forward-
  compat, error paths, the bd-a17114 criterion-5 two-phrase-two-
  finals scenario, the criterion-6 mid-stream reset semantics, and
  pinning `PROTOCOL_VERSION` + `KNOWN_EVENT_TYPES` against
  accidental drift

## Diff summary

- Files: 4 created, 1 modified
  - `Cargo.toml` — added crate to workspace `members`
  - `crates/caco-stt-protocol/Cargo.toml` (new)
  - `crates/caco-stt-protocol/src/lib.rs` (new, ~430 lines incl. tests)
- Tests: +19 / -0
- Behavioural delta: zero — pure addition. No existing crate depends
  on this one yet; consumers will pick it up as their own beads land.

## Operator-takeaway

The wire shape is intentionally tiny and snake_case so a human
operator can `tail -f` an STT stream JSONL file and read along. The
forward-compat `parse_event_lenient` path means future event kinds
(e.g. confidence scores, speaker-id tags) can be added without
breaking existing consumers. The `KNOWN_EVENT_TYPES` test guards
against the typical drift bug (new variant added to enum but lenient
parser still drops it).

`PROTOCOL_VERSION` lives in the `Started` event so consumers detect
drift on connect rather than mid-stream.

The `latency::*` constants are documentation, not enforcement —
engine implementors can opt their own benches into them.
