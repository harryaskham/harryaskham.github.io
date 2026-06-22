# Session summary — surface agent-start errors in caco agent status

## Goal

Stop agent-start extension/MCP/runtime load failures from failing silently: an
agent that starts degraded (e.g. an MCP that failed to load on a tool-name
conflict) should surface an operator-visible signal in `caco agent status`
instead of only emitting to stderr where it is lost.

## Bead(s)

- `bd-fce8b8` — Agent-start extension/MCP load errors fail silently (slice 1:
  the daemon-only, no-Pi-dependency generic startup-error signal)

## Before state

- Startup extension/MCP/runtime load errors went to the agent `wrapper.log` /
  stderr and were not reflected in `caco agent status`, `caco doctor`, or TUI
  agent detail; an agent could start DEGRADED with no signal.
- `caco agent status` (`local_agent_status_json`) had no startup-diagnostics
  field. Adding one to the `AgentInfo` struct is a maximal bd-148dc8 cascade
  (262 literals, no `Default` derive).

## After state

- `caco agent status` now includes an on-demand `startup_diagnostics` object
  ({error_count, sample, reason}) when the agent's `wrapper.log` startup window
  contains error/failure lines; absent otherwise.
- Implemented as a pure, conservative `detect_startup_diagnostics(&str)` helper
  (scans the first 400 lines; matches explicit error-level / failed-to-load /
  failed-to-register / could-not-load / conflicts-with / panicked / fatal
  markers; skips benign "no error"/"0 errors"), surfaced via an additive
  `obj.insert` in `local_agent_status_json` — no `AgentInfo` field, so zero
  struct cascade.

## Diff summary

- Code/content commit: `bd-fce8b8: surface agent-start wrapper.log errors in
  caco agent status (slice 1)`. Final landed squash SHA comes from the
  reintegration receipt.
- Summary artefact commit: intentionally omitted (no self-reference).
- Files touched: `crates/caco-daemon/src/lib.rs` (struct + helper + one
  `obj.insert` + unit test).
- Tests: added `detect_startup_diagnostics_flags_load_failures_and_skips_clean_logs_bd_fce8b8`.
  Validation (gate currently off → self-validated): queued `cargo clippy -p
  caco-daemon` exit 0 + the new unit test passed; the full caco-daemon test
  target compiled.
- Behavioural delta: a degraded agent start is now visible in `caco agent
  status` rather than silent.

## Operator-takeaway

This is slice 1 of bd-fce8b8 — the generic, low-coupling, daemon-only signal
that an agent started with errors in its wrapper.log. It deliberately avoids the
262-literal `AgentInfo` cascade by computing on-demand. The remaining slices
(doctor/TUI surfacing, Pi-specific semantic parsing once the external Pi
error-log format is pinned, and the pi-mcp-adapter vs tendril-share.js
tool-name-collision dedup which lives in the external agent-utils lane) remain
open follow-ons on the bead.
