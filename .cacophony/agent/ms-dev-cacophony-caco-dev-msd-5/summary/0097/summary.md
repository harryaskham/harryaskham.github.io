# Session summary — bd-fce8b8 slice 1b (startup diagnostics in logs response)

## Goal

Extend the slice-1 agent-start error signal so it also reaches the TUI agent
detail surface, not only `caco agent status`.

## Bead(s)

- `bd-fce8b8` — Agent-start extension/MCP load errors fail silently (slice 1b)

## Before state

- After slice 1, `startup_diagnostics` was surfaced only in `caco agent status`.
  The agent logs/diagnostics response (which feeds TUI agent detail) read
  `wrapper_log` but did not expose the structured signal.

## After state

- The agent logs/diagnostics response now includes a `startup_diagnostics` field
  (null when clean) from the same on-demand `detect_startup_diagnostics` helper,
  so TUI agent detail can flag a degraded start.

## Diff summary

- Code/content commit: `bd-fce8b8: surface startup_diagnostics in agent
  logs/diagnostics response (slice 1b)`. Final landed squash SHA from the receipt.
- Summary artefact commit: intentionally omitted (no self-reference).
- Files touched: `crates/caco-daemon/src/lib.rs` (one additive JSON field reusing
  the slice-1 helper).
- Tests: covered by the existing slice-1 detector unit test; this is additive
  wiring. Validation: cargo check -p caco-daemon exit 0.
- Behavioural delta: TUI agent detail can now show startup error diagnostics.

## Operator-takeaway

Small additive follow-on completing the TUI-agent-detail surfacing for bd-fce8b8.
Remaining open follow-ons: the `caco doctor` row (caco-cli aggregator) and the
external Pi/agent-utils slices.
